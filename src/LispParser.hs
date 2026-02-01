module LispParser 
    (   char
    ) where

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

char :: Parser Char
char = undefined