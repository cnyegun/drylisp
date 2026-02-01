{-# LANGUAGE ScopedTypeVariables #-}
module TestLispParser (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import DryLisp
import LispParser
import Data.List

spec :: Spec
spec = describe "LispParser" $ do 
    describe "charP" $ do
        it "fails when given empty string" $ do
            property $ \c -> parse (charP c) "" `shouldBe` Nothing
        it "parses matching character" $ do
            property $ \c rest -> parse (charP c) (c:rest) `shouldBe` Just (c, rest)
        it "fails when meet other char" $ do
            property $ \c c' rest -> 
                c /= c' ==> parse (charP c) (c':rest) `shouldBe` Nothing
    describe "stringP" $ do
        it "parses when parameter is empty string" $ do
            property $ \s -> parse (stringP "") s `shouldBe` Just ("", s)
        it "fails when given empty string" $ do
            property $ \s -> s /= "" ==> parse (stringP s) "" `shouldBe` Nothing
        it "fails when different string" $ do
            property $ \s1 s2 -> 
                not (s1 `isPrefixOf` s2) && s1 /= "" ==> parse (stringP s1) s2 `shouldBe` Nothing
        it "parses when match string" $ do
            parse (stringP "foo") "foobar2000" `shouldBe` Just ("foo", "bar2000")
        it "parses exact match" $ do
            parse (stringP "foo") "foo" `shouldBe` Just ("foo", "")
    describe "stringLiteralP" $ do
        it "fails on empty string" $ do
            parse stringLiteralP "" `shouldBe` Nothing
        it "parses empty string" $ do
            parse stringLiteralP "\"\"" `shouldBe` Just ("", "")
        it "parses on normal string literal" $ do
            parse stringLiteralP "\"eminem\" jayz" `shouldBe` Just ("eminem", " jayz")