{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module LispParser 
    (   parse
    ,   charP
    ,   stringP
    ,   stringLiteralP
    ,   lispBoolP
    ,   lispStringP
    ) where
import DryLisp
import Data.Char
import Control.Applicative

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ \text -> do
        (x, rest) <- parse p text
        Just (f x, rest)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \text -> Just (x, text)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> pv = Parser $ \text -> do 
        (f, rest) <- parse pf text
        (v, rest') <- parse pv rest
        Just (f v, rest')

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const Nothing
    (<|>) :: Parser a -> Parser a -> Parser a
    pa <|> pb = Parser $ \text ->
        case parse pa text of
            Just result -> Just result
            _ -> parse pb text

satisfy :: (Char -> Bool) -> Parser Char
satisfy fcond = Parser $ \case 
    (x:xs) | fcond x -> Just (x, xs)
    _ -> Nothing

charP :: Char -> Parser Char
charP c = satisfy (==c)

stringP :: String -> Parser String
stringP "" = pure ""
stringP (c:cs) = tok $ (:) <$> charP c <*> stringP cs

stringLiteralP :: Parser String
stringLiteralP = tok $ charP '"' *> many (satisfy (/='"')) <* charP '"'

lispBoolP :: Parser LispExpr
lispBoolP = trueP <|> falseP

trueP :: Parser LispExpr
trueP = tok $ (\_ -> LispBool True) <$> stringP "true" 

falseP :: Parser LispExpr
falseP = tok $ (\_ -> LispBool True) <$> stringP "false" 

ws :: Parser String
ws = many (satisfy isSpace)

tok :: Parser a -> Parser a
tok p = p <* ws

lispStringP :: Parser LispExpr
lispStringP = tok $ LispString <$> stringLiteralP 