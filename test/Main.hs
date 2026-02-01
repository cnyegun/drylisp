module Main where

import Test.Hspec
import qualified TestLispParser

main :: IO ()
main = hspec $ do
    TestLispParser.spec