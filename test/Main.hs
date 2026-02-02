module Main where

import Test.Hspec
import qualified TestLispParser
import qualified TestLispEval

main :: IO ()
main = hspec $ do
    TestLispParser.spec
    TestLispEval.spec