import Control.Monad.State
import Data.Char (ord, chr)
import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec
import Prelude hiding (read)
import System.Environment

data Operation = Increment Int              
                | Decrement Int             
                | Next Int                 
                | Back Int           
                | Print
                | Read        
                | Loop [Operation]    
    deriving (Show,Eq)

type Program = [Operation]

data Symbols = Symbols {
    incr :: String,     -- +
    decr :: String,     -- -
    right :: String,    -- >
    left :: String,     -- <
    read :: String,     -- ,
    prt :: String,      -- .
    openl :: String,    -- [
    closel :: String,   -- ]
    reserved :: String  -- []+-<>.,
}

type BFParser = Parser [Operation]

parser :: Symbols -> BFParser
parser s = fmap catMaybes $ many $ try instr <|> try loop <|> r
    where
        r = noneOf (reserved s) >> return Nothing
        instr = choice [
            parseInstr incr (Increment 1),
            parseInstr decr (Decrement 1),
            parseInstr right (Next 1),
            parseInstr left (Back 1),
            parseInstr read Read,
            parseInstr prt Print
            ]
        loop = between
            (string $ openl s)            
            (string $ closel s)           
            (Just . Loop <$> parser s)    
        parseInstr f inst = try $ do
            _ <- string $ f s
            return $ Just inst

program :: BFParser
program = parser Symbols {
    incr     = "+",
    decr     = "-",
    right    = ">",
    left     = "<",
    read     = ",",
    prt      = ".",
    openl    = "[",
    closel   = "]",
    reserved = "[]+-<>.,"
}

data Tape = Tape [Int] Int [Int]

empty :: Tape
empty = Tape (repeat 0) 0 (repeat 0)

increment :: Int -> Tape -> Tape
increment i (Tape xs x ys) = Tape xs (x+i) ys

decrement :: Int -> Tape -> Tape
decrement i (Tape xs x ys) = Tape xs (x-i) ys

moveLeft :: Tape -> Tape
moveLeft t@(Tape [] _ _) = t
moveLeft (Tape (x:xs) p ys) = Tape xs x (p:ys)

moveRight :: Tape -> Tape
moveRight t@(Tape _ _ []) = t
moveRight (Tape xs p (y:ys)) = Tape (p:xs) y ys

setV :: Int -> Tape -> Tape
setV x (Tape xs _ ys) = Tape xs x ys

getV :: Tape -> Int
getV (Tape _ x _) = x

execute :: Operation -> StateT Tape IO()
execute (Increment i) = modify (increment i)
execute (Decrement i) = modify (decrement i)
execute (Next i) = modify ((!! i) . iterate moveRight)
execute (Back i) = modify ((!! i) . iterate moveLeft)
execute Read = liftIO getChar >>= modify . setV . ord
execute Print = gets getV >>= liftIO . putStr . return . chr
execute l@(Loop xs) = do
    x <- gets getV
    unless (x == 0) $ mapM_ execute xs >> execute l

eval :: Program -> IO ()
eval [] = return ()
eval xs = evalStateT (mapM_ execute xs) empty


main :: IO ()
main = do
    (fileName:_) <- getArgs
    result <- parseFromFile program fileName
    case result of
        Left err -> putStr "error: " >> Prelude.print err
        Right x -> eval x