module Main where
import System.Environment
import Text.Regex.Posix
import Data.Strings
import Data.Stack

main :: IO ()

data Token = If | Else | Lparen | Rparen | Semi | Comma | Eq | Type | Id
    deriving (Show, Eq)

data Symbol = S | E | A
    deriving (Show, Eq)

data TokenSymbol t = Token t | Symbol

toToken s
    | s == "if" = If
    | s == "else" = Else
    | s == "(" = Lparen
    | s == ")" = Rparen
    | s == ";" = Semi
    | s == "," = Comma
    | s == "=" = Eq
    | (s =~ "(char|bool|int)" :: Bool) = Type s
    | (s =~ "[a-zA-Z]" :: Bool) = Id s

tokenize str = map toToken (words str)

llParse :: ([Token] -> [Token]) -> [Token]
llParse w stack
   | [] [] = []
    -- | [] [] = []
    -- | (a:w') (x:stack') = case(a,x) of
                            -- (If, S) -> llParse(w', (reverse [If Lparen E Rparen S Semi Else S Semi]) ++ stack')
    -- | (x:w') (x:stack') = llParse(w, stack')
    -- | (a:w') (Terminal _:stack') = error "failed to parse"
    -- | (If:w') (S:stack') = llParse(w', (reverse [If Lparen E Rparen S Semi Else S Semi]) ++ stack')

main = do
   l <- getLine
   let w = tokenize l
   let stack = stackNew
   print(w)
