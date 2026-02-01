module TestLispParser (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
-- import DryLisp

spec :: Spec
spec = describe "LispParser" $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException :: IO ()

