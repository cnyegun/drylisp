module Main where

import DryLisp (eval, initialEnv, Env)
import LispParser (parse, lispExprP)
import Data.Char (isSpace)
import System.IO (hFlush, stdout)

repl :: Env -> IO ()
repl env = do
    putStr "drylisp> "
    hFlush stdout
    input <- getLine
    
    if input `elem` ["quit", "exit", ""]
        then return ()
        else do
            let trimmed = dropWhile isSpace input
            
            case parse lispExprP trimmed of
                Nothing -> do
                    putStrLn "Parse Error"
                    repl env
                Just (expr, _) -> do
                    
                    case eval env expr of
                        Left err -> do
                            putStrLn $ "Error: " ++ err
                            repl env
                        Right (newEnv, val) -> do
                            print val
                            repl newEnv

main :: IO ()
main = do
    putStrLn "drylisp 0.1.0.0 (there is no --help :) )"
    repl initialEnv