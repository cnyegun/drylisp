{-# LANGUAGE ScopedTypeVariables #-}
module TestLispEval (spec) where

import Test.Hspec
import Test.QuickCheck
import DryLisp

spec :: Spec
spec = describe "TestLispEval" $ do
    it "eval bool" $ do
        eval [] (LispBool True) `shouldBe` Right (LispBool True)
        eval [] (LispBool False) `shouldBe` Right (LispBool False)
    it "eval numbers" $ do 
        property  $ \(n :: Double) ->
            eval [] (LispNumber n) `shouldBe` Right (LispNumber n)
    it "eval strings" $ do
        property $ \(s :: String) ->
            eval [] (LispString s) `shouldBe` Right (LispString s)
    it "eval id -> simple name" $ do
        eval [("x", LispNumber 42)] (Id "x") `shouldBe` Right (LispNumber 42)
        eval [("y", LispNumber 46)] (Id "x") `shouldBe` Left ("Unbound variable: " ++ "x")
    it "eval quote -> expression unevaluated" $ do
        eval [] (List [Id "quote", Id "x"]) `shouldBe` Right (Id "x")
        eval [] (List [Id "quote", List [LispNumber 1, LispNumber 2]]) 
            `shouldBe` Right (List [LispNumber 1, LispNumber 2]) 

    it "eval if -> true condition evaluates then-branch" $ do
        eval [] (List [Id "if", LispBool True, LispNumber 1, LispNumber 2]) 
            `shouldBe` Right (LispNumber 1)
        eval [] (List [Id "if", LispNumber 0, LispString "yes", LispString "no"])
            `shouldBe` Right (LispString "yes")

    it "eval if -> false condition evaluates else-branch" $ do
        eval [] (List [Id "if", LispBool False, LispNumber 1, LispNumber 2])
            `shouldBe` Right (LispNumber 2)

    it "eval if -> short-circuits (only evaluates chosen branch)" $ do
        -- If else-branch were evaluated, this would be "Unbound variable: x"
        eval [] (List [Id "if", LispBool True, LispNumber 42, Id "x"])
            `shouldBe` Right (LispNumber 42)
        eval [] (List [Id "if", LispBool False, Id "y", LispNumber 99])
            `shouldBe` Right (LispNumber 99)

    it "eval if -> condition can use variables" $ do
        eval [("flag", LispBool True)] 
            (List [Id "if", Id "flag", LispNumber 1, LispNumber 0])
            `shouldBe` Right (LispNumber 1)

    it "eval if -> nested ifs work" $ do
        -- (if #t (if #f 1 2) 3) => 2
        let nested = List [Id "if", LispBool True, 
                        List [Id "if", LispBool False, LispNumber 1, LispNumber 2],
                        LispNumber 3]
        eval [] nested `shouldBe` Right (LispNumber 2)

    it "eval if -> wrong number of args is error" $ do
        eval [] (List [Id "if", LispBool True]) `shouldBe` Left "if requires exactly 3 arguments"
        eval [] (List [Id "if", LispBool True, LispNumber 1]) `shouldBe` Left "if requires exactly 3 arguments"
        eval [] (List [Id "if", LispBool True, LispNumber 1, LispNumber 2, LispNumber 3])
            `shouldBe` Left "if requires exactly 3 arguments"

    it "lispAdd -> adds two numbers" $ do
        eval initialEnv (List [Id "+", LispNumber 1, LispNumber 2])
                `shouldBe` Right (LispNumber 3)

    it "lispAdd -> adds multiple numbers" $ do
        eval initialEnv (List [Id "+", LispNumber 1, LispNumber 2, LispNumber 3])
            `shouldBe` Right (LispNumber 6)

    it "lispAdd -> single number returns itself" $ do
        eval initialEnv (List [Id "+", LispNumber 42])
            `shouldBe` Right (LispNumber 42)

    it "lispAdd -> fails on string argument" $ do
        eval initialEnv (List [Id "+", LispNumber 1, LispString "hello"])
            `shouldBe` Left "+ requires numbers"

    it "lispAdd -> fails on unbound variable in args" $ do
        eval initialEnv (List [Id "+", LispNumber 1, Id "undefined"])
            `shouldBe` Left "Unbound variable: undefined"

    it "lispAdd -> works with nested calls" $ do
        -- (+ 1 (+ 2 3))
        eval initialEnv (List [Id "+", LispNumber 1, 
                            List [Id "+", LispNumber 2, LispNumber 3]])
            `shouldBe` Right (LispNumber 6)
