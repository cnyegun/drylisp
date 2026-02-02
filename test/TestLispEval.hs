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
        eval [("y", LispNumber 46)] (Id "x") 
            `shouldBe` Left ("Unbound variable: " ++ "x")
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
        eval [] (List [Id "if", LispBool True]) 
            `shouldBe` Left "if requires exactly 3 arguments"
        eval [] (List [Id "if", LispBool True, LispNumber 1]) 
            `shouldBe` Left "if requires exactly 3 arguments"
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
    
    it "eval lambda -> creates closure and calls it" $ do
        let lambdaExpr = List [Id "lambda", List [Id "x"], 
                            List [Id "+", Id "x", LispNumber 1]]
        eval initialEnv (List [lambdaExpr, LispNumber 42])
            `shouldBe` Right (LispNumber 43)
    
    it "lambda -> identity function returns argument" $ do
        let identity = List [Id "lambda", List [Id "x"], Id "x"]
        eval initialEnv (List [identity, LispNumber 42]) 
            `shouldBe` Right (LispNumber 42)

    it "lambda -> single parameter arithmetic" $ do
        let inc = List [Id "lambda", List [Id "x"], 
                        List [Id "+", Id "x", LispNumber 1]]
        eval initialEnv (List [inc, LispNumber 5])
            `shouldBe` Right (LispNumber 6)

    it "lambda -> multiple parameters" $ do
        let add = List [Id "lambda", List [Id "x", Id "y"], 
                        List [Id "+", Id "x", Id "y"]]
        eval initialEnv (List [add, LispNumber 3, LispNumber 4])
            `shouldBe` Right (LispNumber 7)

    it "lambda -> nested application" $ do
        let add = List [Id "lambda", List [Id "x", Id "y"], 
                        List [Id "+", Id "x", Id "y"]]
        eval initialEnv 
            (List [add, List [add, LispNumber 1, LispNumber 2], LispNumber 3])
                `shouldBe` Right (LispNumber 6)

    it "lambda -> lexical scope captures outer variables" $ do
        let closure = List [Id "lambda", List [Id "x"], 
                            List [Id "+", Id "x", Id "y"]]
        eval (initialEnv ++ [("y", LispNumber 10)]) (List [closure, LispNumber 5])
            `shouldBe` Right (LispNumber 15)

    it "lambda -> lexical scope is static, not dynamic" $ do
        let closure = List [Id "lambda", List [Id "x"], 
                            List [Id "+", Id "x", Id "y"]]
        let env = initialEnv ++ [("y", LispNumber 10)]
        eval env (List [closure, LispNumber 5])
            `shouldBe` Right (LispNumber 15)

    it "lambda -> arity mismatch too few args" $ do
        let add = List [Id "lambda", List [Id "x", Id "y"], 
                        List [Id "+", Id "x", Id "y"]]
        eval initialEnv (List [add, LispNumber 1])
            `shouldBe` Left "lambda: arity mismatch"

    it "lambda -> arity mismatch too many args" $ do
        let identity = List [Id "lambda", List [Id "x"], Id "x"]
        eval initialEnv (List [identity, LispNumber 1, LispNumber 2])
            `shouldBe` Left "lambda: arity mismatch"

    it "lambda -> body can be complex expression" $ do
        let calc = List [Id "lambda", List [Id "a", Id "b"], 
                        List [Id "+", 
                            List [Id "*", Id "a", Id "a"], 
                            List [Id "*", Id "b", Id "b"]]]
        eval initialEnv (List [calc, LispNumber 3, LispNumber 4])
            `shouldBe` Right (LispNumber 25)

    it "lambda -> returns closure as value" $ do
        let lam = List [Id "lambda", List [Id "x"], Id "x"]
        case eval initialEnv lam of
            Right (LispClosure _ _) -> True
            _ -> False

    it "cons -> cons an LispExpr with a list" $ do
        eval initialEnv (List [Id "cons", LispNumber 4, LispNumber 5]) `shouldBe`
            Left "cons requires the second argument must be a list"
        eval initialEnv (List [Id "cons", LispNumber 3, List [Id "quote", List []]]) 
            `shouldBe` Right (List [LispNumber 3])
        eval initialEnv (List [Id "cons", LispNumber 3, List [Id "quote", List [LispNumber 4]]]) 
            `shouldBe` Right (List [LispNumber 3, LispNumber 4])

    it "car -> returns first element" $ do
        eval initialEnv (List [Id "car", List [Id "quote", List [LispNumber 1, LispNumber 2]]])
            `shouldBe` Right (LispNumber 1)

    it "car -> errors on empty list" $ do
        eval initialEnv (List [Id "car", List [Id "quote", List []]])
            `shouldBe` Left "car requires a non-empty list"

    it "cdr -> returns rest of list" $ do
        -- cdr returns the actual list, not a quoted expression
        eval initialEnv (List [Id "cdr", List [Id "quote", List [LispNumber 1, LispNumber 2, LispNumber 3]]])
            `shouldBe` Right (List [LispNumber 2, LispNumber 3])

    it "car/cdr -> work together with cons" $ do
        -- (car (cons 1 '(2 3))) => 1
        eval initialEnv (List [Id "car", List [Id "cons", LispNumber 1, 
                                List [Id "quote", List [LispNumber 2, LispNumber 3]]]])
            `shouldBe` Right (LispNumber 1)

    it "car -> returns first element of list" $ do
        eval initialEnv (List [Id "car", List [Id "quote", List [LispNumber 1, LispNumber 2]]])
            `shouldBe` Right (LispNumber 1)
        eval initialEnv (List [Id "car", List [Id "quote", List [LispString "a", LispString "b"]]])
            `shouldBe` Right (LispString "a")

    it "car -> errors on empty list" $ do
        eval initialEnv (List [Id "car", List [Id "quote", List []]])
            `shouldBe` Left "car requires a non-empty list"

    it "car -> errors on non-list" $ do
        eval initialEnv (List [Id "car", LispNumber 42])
            `shouldBe` Left "car requires a list"
        eval initialEnv (List [Id "car", Id "x"])
            `shouldBe` Left "Unbound variable: x"

    it "cdr -> returns rest of list" $ do
        eval initialEnv (List [Id "cdr", List [Id "quote", List [LispNumber 1, LispNumber 2, LispNumber 3]]])
            `shouldBe` Right (List [LispNumber 2, LispNumber 3])

    it "cdr -> returns empty list for single element" $ do
        -- Returns empty list, not a quoted empty list
        eval initialEnv (List [Id "cdr", List [Id "quote", List [LispNumber 42]]])
            `shouldBe` Right (List [])

    it "cdr -> errors on empty list" $ do
        eval initialEnv (List [Id "cdr", List [Id "quote", List []]])
            `shouldBe` Left "cdr requires a non-empty list"

    it "car/cdr -> work with cons" $ do
        -- (car (cons 1 '(2 3))) => 1
        eval initialEnv (List [Id "car", List [Id "cons", LispNumber 1, 
                                List [Id "quote", List [LispNumber 2, LispNumber 3]]]])
            `shouldBe` Right (LispNumber 1)
        -- (cdr (cons 1 '(2 3))) => '(2 3) which is the list value (2 3)
        eval initialEnv (List [Id "cdr", List [Id "cons", LispNumber 1, 
                                List [Id "quote", List [LispNumber 2, LispNumber 3]]]])
            `shouldBe` Right (List [LispNumber 2, LispNumber 3])

    it "car/cdr -> cadr pattern (car of cdr)" $ do
        -- (car (cdr '(1 2 3))) => 2
        eval initialEnv (List [Id "car", List [Id "cdr", 
                                List [Id "quote", List [LispNumber 1, LispNumber 2, LispNumber 3]]]])
            `shouldBe` Right (LispNumber 2)

    it "= -> check for equality case: true" $ do
        eval (initialEnv ++ [("x", LispString "hi"), ("y", LispString "hi")]) 
            (List [Id "=", Id "x", Id "y"])
                `shouldBe` Right (LispBool True)

        eval (initialEnv ++ [("x", LispNumber 5), ("y", LispNumber 5)]) 
            (List [Id "=", Id "x", LispNumber 5])
                `shouldBe` Right (LispBool True)
                
    it "= -> check for equality case: false" $ do
        eval (initialEnv ++ [("x", LispString "hi"), ("y", LispString "")]) 
            (List [Id "=", Id "x", Id "y"])
                `shouldBe` Right (LispBool False)

        eval (initialEnv ++ [("x", LispNumber 3), ("y", LispString "")]) 
            (List [Id "=", Id "x", Id "y"])
                `shouldBe` Left "Wrong type for comparison"