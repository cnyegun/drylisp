{-# LANGUAGE ScopedTypeVariables #-}
module TestLispEval (spec) where

import Test.Hspec
import Test.QuickCheck
import DryLisp

spec :: Spec
spec = describe "TestLispEval" $ do
    it "eval bool" $ do
        eval [] (LispBool True) `shouldBe` Right ([], LispBool True)
        eval [] (LispBool False) `shouldBe` Right ([], LispBool False)
    it "eval numbers" $ do 
        property  $ \(n :: Double) ->
            eval [] (LispNumber n) `shouldBe` Right ([], LispNumber n)
    it "eval strings" $ do
        property $ \(s :: String) ->
            eval [] (LispString s) `shouldBe` Right ([], LispString s)
    it "eval id -> simple name" $ do
        eval [("x", LispNumber 42)] (Id "x") `shouldBe` Right ([("x", LispNumber 42)], LispNumber 42)
        eval [("y", LispNumber 46)] (Id "x") 
            `shouldBe` Left ("Unbound variable: " ++ "x")
    it "eval quote -> expression unevaluated" $ do
        eval [] (List [Id "quote", Id "x"]) `shouldBe` Right ([], Id "x")
        eval [] (List [Id "quote", List [LispNumber 1, LispNumber 2]]) 
            `shouldBe` Right ([], List [LispNumber 1, LispNumber 2]) 

    it "eval if -> true condition evaluates then-branch" $ do
        eval [] (List [Id "if", LispBool True, LispNumber 1, LispNumber 2]) 
            `shouldBe` Right ([], LispNumber 1)
        eval [] (List [Id "if", LispNumber 0, LispString "yes", LispString "no"])
            `shouldBe` Right ([], LispString "yes")

    it "eval if -> false condition evaluates else-branch" $ do
        eval [] (List [Id "if", LispBool False, LispNumber 1, LispNumber 2])
            `shouldBe` Right ([], LispNumber 2)

    it "eval if -> short-circuits (only evaluates chosen branch)" $ do
        -- If else-branch were evaluated, this would be "Unbound variable: x"
        eval [] (List [Id "if", LispBool True, LispNumber 42, Id "x"])
            `shouldBe` Right ([], LispNumber 42)
        eval [] (List [Id "if", LispBool False, Id "y", LispNumber 99])
            `shouldBe` Right ([], LispNumber 99)

    it "eval if -> condition can use variables" $ do
        eval [("flag", LispBool True)] 
            (List [Id "if", Id "flag", LispNumber 1, LispNumber 0])
            `shouldBe` Right ([("flag", LispBool True)], LispNumber 1)

    it "eval if -> nested ifs work" $ do
        -- (if #t (if #f 1 2) 3) => 2
        let nested = List [Id "if", LispBool True, 
                        List [Id "if", LispBool False, LispNumber 1, LispNumber 2],
                        LispNumber 3]
        eval [] nested `shouldBe` Right ([], LispNumber 2)

    it "eval if -> wrong number of args is error" $ do
        eval [] (List [Id "if", LispBool True]) 
            `shouldBe` Left "if requires exactly 3 arguments"
        eval [] (List [Id "if", LispBool True, LispNumber 1]) 
            `shouldBe` Left "if requires exactly 3 arguments"
        eval [] (List [Id "if", LispBool True, LispNumber 1, LispNumber 2, LispNumber 3])
            `shouldBe` Left "if requires exactly 3 arguments"

    it "lispAdd -> adds two numbers" $ do
        eval initialEnv (List [Id "+", LispNumber 1, LispNumber 2])
                `shouldBe` Right (initialEnv, LispNumber 3)

    it "lispAdd -> adds multiple numbers" $ do
        eval initialEnv (List [Id "+", LispNumber 1, LispNumber 2, LispNumber 3])
            `shouldBe` Right (initialEnv, LispNumber 6)

    it "lispAdd -> single number returns itself" $ do
        eval initialEnv (List [Id "+", LispNumber 42])
            `shouldBe` Right (initialEnv, LispNumber 42)

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
            `shouldBe` Right (initialEnv, LispNumber 6)
    
    it "eval lambda -> creates closure and calls it" $ do
        let lambdaExpr = List [Id "lambda", List [Id "x"], 
                            List [Id "+", Id "x", LispNumber 1]]
        eval initialEnv (List [lambdaExpr, LispNumber 42])
            `shouldBe` Right (initialEnv, LispNumber 43)
    
    it "lambda -> identity function returns argument" $ do
        let identity = List [Id "lambda", List [Id "x"], Id "x"]
        eval initialEnv (List [identity, LispNumber 42]) 
            `shouldBe` Right (initialEnv, LispNumber 42)

    it "lambda -> single parameter arithmetic" $ do
        let inc = List [Id "lambda", List [Id "x"], 
                        List [Id "+", Id "x", LispNumber 1]]
        eval initialEnv (List [inc, LispNumber 5])
            `shouldBe` Right (initialEnv, LispNumber 6)

    it "lambda -> multiple parameters" $ do
        let add = List [Id "lambda", List [Id "x", Id "y"], 
                        List [Id "+", Id "x", Id "y"]]
        eval initialEnv (List [add, LispNumber 3, LispNumber 4])
            `shouldBe` Right (initialEnv, LispNumber 7)

    it "lambda -> nested application" $ do
        let add = List [Id "lambda", List [Id "x", Id "y"], 
                        List [Id "+", Id "x", Id "y"]]
        eval initialEnv 
            (List [add, List [add, LispNumber 1, LispNumber 2], LispNumber 3])
                `shouldBe` Right (initialEnv, LispNumber 6)

    it "lambda -> lexical scope captures outer variables" $ do
        let closure = List [Id "lambda", List [Id "x"], 
                            List [Id "+", Id "x", Id "y"]]
        eval (initialEnv ++ [("y", LispNumber 10)]) (List [closure, LispNumber 5])
            `shouldBe` Right (initialEnv ++ [("y", LispNumber 10)], LispNumber 15)

    it "lambda -> lexical scope is static, not dynamic" $ do
        let closure = List [Id "lambda", List [Id "x"], 
                            List [Id "+", Id "x", Id "y"]]
        let env = initialEnv ++ [("y", LispNumber 10)]
        eval env (List [closure, LispNumber 5])
            `shouldBe` Right (env, LispNumber 15)

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
            `shouldBe` Right (initialEnv, LispNumber 25)

    it "lambda -> returns closure as value" $ do
        let lam = List [Id "lambda", List [Id "x"], Id "x"]
        case eval initialEnv lam of
            Right (_, LispClosure _ _) -> True
            _ -> False

    it "cons -> cons an LispExpr with a list" $ do
        eval initialEnv (List [Id "cons", LispNumber 4, LispNumber 5]) `shouldBe`
            Left "cons requires the second argument must be a list"
        eval initialEnv (List [Id "cons", LispNumber 3, List [Id "quote", List []]]) 
            `shouldBe` Right (initialEnv, List [LispNumber 3])
        eval initialEnv (List [Id "cons", LispNumber 3, List [Id "quote", List [LispNumber 4]]]) 
            `shouldBe` Right (initialEnv, List [LispNumber 3, LispNumber 4])

    it "car -> returns first element" $ do
        eval initialEnv (List [Id "car", List [Id "quote", List [LispNumber 1, LispNumber 2]]])
            `shouldBe` Right (initialEnv, LispNumber 1)

    it "car -> errors on empty list" $ do
        eval initialEnv (List [Id "car", List [Id "quote", List []]])
            `shouldBe` Left "car requires a non-empty list"

    it "cdr -> returns rest of list" $ do
        -- cdr returns the actual list, not a quoted expression
        eval initialEnv (List [Id "cdr", List [Id "quote", List [LispNumber 1, LispNumber 2, LispNumber 3]]])
            `shouldBe` Right (initialEnv, List [LispNumber 2, LispNumber 3])

    it "car/cdr -> work together with cons" $ do
        -- (car (cons 1 '(2 3))) => 1
        eval initialEnv (List [Id "car", List [Id "cons", LispNumber 1, 
                                List [Id "quote", List [LispNumber 2, LispNumber 3]]]])
            `shouldBe` Right (initialEnv, LispNumber 1)

    it "car -> returns first element of list" $ do
        eval initialEnv (List [Id "car", List [Id "quote", List [LispNumber 1, LispNumber 2]]])
            `shouldBe` Right (initialEnv, LispNumber 1)
        eval initialEnv (List [Id "car", List [Id "quote", List [LispString "a", LispString "b"]]])
            `shouldBe` Right (initialEnv, LispString "a")

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
            `shouldBe` Right (initialEnv, List [LispNumber 2, LispNumber 3])

    it "cdr -> returns empty list for single element" $ do
        -- Returns empty list, not a quoted empty list
        eval initialEnv (List [Id "cdr", List [Id "quote", List [LispNumber 42]]])
            `shouldBe` Right (initialEnv, List [])

    it "cdr -> errors on empty list" $ do
        eval initialEnv (List [Id "cdr", List [Id "quote", List []]])
            `shouldBe` Left "cdr requires a non-empty list"

    it "car/cdr -> work with cons" $ do
        -- (car (cons 1 '(2 3))) => 1
        eval initialEnv (List [Id "car", List [Id "cons", LispNumber 1, 
                                List [Id "quote", List [LispNumber 2, LispNumber 3]]]])
            `shouldBe` Right (initialEnv, LispNumber 1)
        -- (cdr (cons 1 '(2 3))) => '(2 3) which is the list value (2 3)
        eval initialEnv (List [Id "cdr", List [Id "cons", LispNumber 1, 
                                List [Id "quote", List [LispNumber 2, LispNumber 3]]]])
            `shouldBe` Right (initialEnv, List [LispNumber 2, LispNumber 3])

    it "car/cdr -> cadr pattern (car of cdr)" $ do
        -- (car (cdr '(1 2 3))) => 2
        eval initialEnv (List [Id "car", List [Id "cdr", 
                                List [Id "quote", List [LispNumber 1, LispNumber 2, LispNumber 3]]]])
            `shouldBe` Right (initialEnv, LispNumber 2)

    it "= -> check for equality case: true" $ do
        eval (initialEnv ++ [("x", LispString "hi"), ("y", LispString "hi")]) 
            (List [Id "=", Id "x", Id "y"])
                `shouldBe` Right (initialEnv ++ [("x", LispString "hi"), ("y", LispString "hi")], LispBool True)

        eval (initialEnv ++ [("x", LispNumber 5), ("y", LispNumber 5)]) 
            (List [Id "=", Id "x", LispNumber 5])
                `shouldBe` Right (initialEnv ++ [("x", LispNumber 5), ("y", LispNumber 5)], LispBool True)
                
    it "= -> check for equality case: false" $ do
        eval (initialEnv ++ [("x", LispString "hi"), ("y", LispString "")]) 
            (List [Id "=", Id "x", Id "y"])
                `shouldBe` Right (initialEnv ++ [("x", LispString "hi"), ("y", LispString "")], LispBool False)

        eval (initialEnv ++ [("x", LispNumber 3), ("y", LispString "")]) 
            (List [Id "=", Id "x", Id "y"])
                `shouldBe` Left "Wrong type for comparison"
    
    it "checks for empty list" $ do
        eval initialEnv (List [Id "empty?", LispNumber 3])
            `shouldBe` Left "Not a list"

        eval initialEnv (List [Id "empty?", List [Id "quote", List []]])
            `shouldBe` Right (initialEnv, LispBool True)

        eval initialEnv (List [Id "empty?", List [Id "quote", List [LispNumber 3]]])
            `shouldBe` Right (initialEnv, LispBool False)

        eval initialEnv (List [Id "null?", List [Id "quote", List [LispNumber 3]]])
            `shouldBe` Right (initialEnv, LispBool False)

    it "> -> greater than true" $ do
        eval initialEnv (List [Id ">", LispNumber 5, LispNumber 3])
            `shouldBe` Right (initialEnv, LispBool True)
        eval (("x", LispNumber 10) : initialEnv) (List [Id ">", Id "x", LispNumber 5])
            `shouldBe` Right (("x", LispNumber 10) : initialEnv, LispBool True)

    it "> -> greater than false" $ do
        eval initialEnv (List [Id ">", LispNumber 2, LispNumber 5])
            `shouldBe` Right (initialEnv, LispBool False)
        eval initialEnv (List [Id ">", LispNumber 5, LispNumber 5])
            `shouldBe` Right (initialEnv, LispBool False)

    it "< -> less than true" $ do
        eval initialEnv (List [Id "<", LispNumber 3, LispNumber 5])
            `shouldBe` Right (initialEnv, LispBool True)

    it "< -> less than false" $ do
        eval initialEnv (List [Id "<", LispNumber 5, LispNumber 3])
            `shouldBe` Right (initialEnv, LispBool False)
        eval initialEnv (List [Id "<", LispNumber 5, LispNumber 5])
            `shouldBe` Right (initialEnv, LispBool False)

    it ">= -> greater than or equal true" $ do
        eval initialEnv (List [Id ">=", LispNumber 5, LispNumber 3])
            `shouldBe` Right (initialEnv, LispBool True)
        eval initialEnv (List [Id ">=", LispNumber 5, LispNumber 5])
            `shouldBe` Right (initialEnv, LispBool True)

    it ">= -> greater than or equal false" $ do
        eval initialEnv (List [Id ">=", LispNumber 2, LispNumber 5])
            `shouldBe` Right (initialEnv, LispBool False)

    it "<= -> less than or equal true" $ do
        eval initialEnv (List [Id "<=", LispNumber 3, LispNumber 5])
            `shouldBe` Right (initialEnv, LispBool True)
        eval initialEnv (List [Id "<=", LispNumber 5, LispNumber 5])
            `shouldBe` Right (initialEnv, LispBool True)

    it "<= -> less than or equal false" $ do
        eval initialEnv (List [Id "<=", LispNumber 5, LispNumber 3])
            `shouldBe` Right (initialEnv, LispBool False)

    it "numeric comparisons -> strict type checking" $ do
        eval initialEnv (List [Id ">", LispNumber 5, LispString "3"])
            `shouldBe` Left "> requires exactly 2 numbers"
        eval initialEnv (List [Id "<", LispString "a", LispNumber 1])
            `shouldBe` Left "< requires exactly 2 numbers"
        eval initialEnv (List [Id ">=", LispBool True, LispNumber 0])
            `shouldBe` Left ">= requires exactly 2 numbers"

    it "numeric comparisons -> wrong arity" $ do
        eval initialEnv (List [Id ">", LispNumber 5])
            `shouldBe` Left "> requires exactly 2 numbers"
        eval initialEnv (List [Id "<", LispNumber 1, LispNumber 2, LispNumber 3])
            `shouldBe` Left "< requires exactly 2 numbers"

    it "numeric comparisons -> work in if expressions" $ do
        eval initialEnv (List [Id "if", 
                    List [Id ">", LispNumber 5, LispNumber 3],
                    LispNumber 1,
                    LispNumber 0])
            `shouldBe` Right (initialEnv, LispNumber 1)
        eval initialEnv (List [Id "if",
                    List [Id "<=", LispNumber 2, LispNumber 1],
                    LispNumber 1,
                    LispNumber 0])
            `shouldBe` Right (initialEnv, LispNumber 0)
    
    it "let -> single binding" $ do
        eval initialEnv (List [Id "let", List [List [Id "x", LispNumber 5]], Id "x"])
            `shouldBe` Right (initialEnv, LispNumber 5)

    it "let -> multiple bindings (parallel)" $ do
        -- (let ([x 1] [y 2]) (+ x y)) => 3
        eval initialEnv (List [Id "let", 
            List [List [Id "x", LispNumber 1], List [Id "y", LispNumber 2]],
            List [Id "+", Id "x", Id "y"]])
            `shouldBe` Right (initialEnv, LispNumber 3)

    it "let -> bindings don't see each other (parallel scoping)" $ do
        -- (let ([x 1] [y (+ x 1)]) y) should fail - x not visible in y's init
        eval initialEnv (List [Id "let",
            List [List [Id "x", LispNumber 1], List [Id "y", List [Id "+", Id "x", LispNumber 1]]],
            Id "y"])
            `shouldBe` Left "Unbound variable: x"

    it "let -> body sees all bindings" $ do
        eval initialEnv (List [Id "let",
            List [List [Id "a", LispNumber 10], List [Id "b", LispNumber 20]],
            List [Id "+", Id "a", Id "b"]])
            `shouldBe` Right (initialEnv, LispNumber 30)

    it "let -> shadows outer scope" $ do
        -- (let ([x 100]) x) inside env where x=42
        eval [("x", LispNumber 42)] (List [Id "let",
            List [List [Id "x", LispNumber 100]],
            Id "x"])
            `shouldBe` Right ([("x", LispNumber 42)], LispNumber 100)

    it "let -> outer scope unchanged after let" $ do
        -- (let ([x 999]) x) returns 999, but env still has original x
        eval [("x", LispNumber 1)] (List [Id "let",
            List [List [Id "x", LispNumber 999]],
            Id "x"])
            `shouldBe` Right ([("x", LispNumber 1)], LispNumber 999)

    it "let -> empty binding list" $ do
        eval initialEnv (List [Id "let", List [], LispNumber 42])
            `shouldBe` Right (initialEnv, LispNumber 42)

    it "let -> complex expressions in bindings" $ do
        -- (let ([x (+ 1 2)] [y (* 3 4)]) (+ x y))
        eval initialEnv (List [Id "let",
            List [List [Id "x", List [Id "+", LispNumber 1, LispNumber 2]],
                  List [Id "y", List [Id "*", LispNumber 3, LispNumber 4]]],
            List [Id "+", Id "x", Id "y"]])
            `shouldBe` Right (initialEnv, LispNumber 15)

    it "let -> error on malformed binding (not a list)" $ do
        eval initialEnv (List [Id "let", List [LispNumber 42], LispNumber 1])
            `shouldBe` Left "Invalid let binding syntax"

    it "let -> error on malformed binding (wrong length)" $ do
        eval initialEnv (List [Id "let", List [List [Id "x"]], Id "x"])
            `shouldBe` Left "Invalid let binding syntax"

    it "let -> error on unbound variable in body" $ do
        eval initialEnv (List [Id "let", List [List [Id "x", LispNumber 1]], Id "y"])
            `shouldBe` Left "Unbound variable: y"

    it "let* -> sequential bindings see previous" $ do
        -- (let* ([x 1] [y (+ x 1)]) y) => 2
        eval initialEnv (List [Id "let*",
            List [List [Id "x", LispNumber 1], 
                  List [Id "y", List [Id "+", Id "x", LispNumber 1]]],
            Id "y"])
            `shouldBe` Right (initialEnv, LispNumber 2)

    it "let* -> multiple sequential dependencies" $ do
        -- (let* ([x 1] [y (+ x 1)] [z (+ x y)]) z) => 3
        eval initialEnv (List [Id "let*",
            List [List [Id "x", LispNumber 1],
                  List [Id "y", List [Id "+", Id "x", LispNumber 1]],
                  List [Id "z", List [Id "+", Id "x", Id "y"]]],
            Id "z"])
            `shouldBe` Right (initialEnv, LispNumber 3)

    it "let* -> later bindings shadow earlier in same let*" $ do
        -- (let* ([x 1] [x (+ x 1)]) x) => 2
        eval initialEnv (List [Id "let*",
            List [List [Id "x", LispNumber 1],
                  List [Id "x", List [Id "+", Id "x", LispNumber 1]]],
            Id "x"])
            `shouldBe` Right (initialEnv, LispNumber 2)

    it "let* -> sees outer scope before shadowing" $ do
        eval (("x", LispNumber 10) : initialEnv) (List [Id "let*",
            List [List [Id "x", List [Id "+", Id "x", LispNumber 1]]],
            Id "x"])
            `shouldBe` Right ((("x", LispNumber 10) : initialEnv), LispNumber 11)

    it "let* -> single binding works same as let" $ do
        eval initialEnv (List [Id "let*",
            List [List [Id "x", LispNumber 5]],
            Id "x"])
            `shouldBe` Right (initialEnv, LispNumber 5)

    it "let* -> empty binding list" $ do
        eval initialEnv (List [Id "let*", List [], LispNumber 42])
            `shouldBe` Right (initialEnv, LispNumber 42)

    it "let* -> cannot reference later binding" $ do
        -- (let* ([x y] [y 1]) x) => error
        eval initialEnv (List [Id "let*",
            List [List [Id "x", Id "y"],
                  List [Id "y", LispNumber 1]],
            Id "x"])
            `shouldBe` Left "Unbound variable: y"

    it "let* -> lambda in early binding sees outer scope not later bindings" $ do
        eval (("x", LispNumber 100) : initialEnv) (List [Id "let*",
            List [List [Id "f", List [Id "lambda", List [Id "n"], 
                                    List [Id "+", Id "x", Id "n"]]],
                List [Id "x", LispNumber 10]],
            List [Id "f", LispNumber 5]])
            `shouldBe` Right ((("x", LispNumber 100) : initialEnv), LispNumber 105)

    it "let* -> outer scope unchanged after let*" $ do
        eval [("a", LispNumber 1)] (List [Id "let*",
            List [List [Id "a", LispNumber 999],
                  List [Id "b", Id "a"]],
            Id "b"])
            `shouldBe` Right ([("a", LispNumber 1)], LispNumber 999)

    it "let* -> complex nested arithmetic" $ do
        -- (let* ([x 5] [y (* x 2)] [z (+ x y)]) z) => 15
        eval initialEnv (List [Id "let*",
            List [List [Id "x", LispNumber 5],
                  List [Id "y", List [Id "*", Id "x", LispNumber 2]],
                  List [Id "z", List [Id "+", Id "x", Id "y"]]],
            Id "z"])
            `shouldBe` Right (initialEnv, LispNumber 15)

    it "let* -> error on malformed binding" $ do
        eval initialEnv (List [Id "let*", List [LispNumber 42], LispNumber 1])
            `shouldBe` Left "Invalid let binding syntax"

    it "letrec -> single recursive function (factorial)" $ do
        -- (letrec ([fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))]) (fact 5)) => 120
        eval initialEnv (List [Id "letrec",
            List [List [Id "fact", List [Id "lambda", List [Id "n"],
                List [Id "if", List [Id "=", Id "n", LispNumber 0],
                    LispNumber 1,
                    List [Id "*", Id "n", 
                        List [Id "fact", List [Id "-", Id "n", LispNumber 1]]]]]]],
            List [Id "fact", LispNumber 5]])
            `shouldBe` Right (initialEnv, LispNumber 120)

    it "letrec -> mutually recursive even?/odd?" $ do
        -- (letrec ([even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))]
        --          [odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))])
        --   (even? 4))
        eval initialEnv (List [Id "letrec",
            List [List [Id "even?", List [Id "lambda", List [Id "n"],
                            List [Id "if", List [Id "=", Id "n", LispNumber 0],
                                LispBool True,
                                List [Id "odd?", List [Id "-", Id "n", LispNumber 1]]]]],
                  List [Id "odd?", List [Id "lambda", List [Id "n"],
                            List [Id "if", List [Id "=", Id "n", LispNumber 0],
                                LispBool False,
                                List [Id "even?", List [Id "-", Id "n", LispNumber 1]]]]]],
            List [Id "even?", LispNumber 4]])
            `shouldBe` Right (initialEnv, LispBool True)

    it "letrec -> mutual recursion with different arities" $ do
        -- (letrec ([inc (lambda (x) (+ x 1))]
        --           [dec (lambda (x) (- x 1))])
        --   (dec (inc 5)))
        eval initialEnv (List [Id "letrec",
            List [List [Id "inc", List [Id "lambda", List [Id "x"],
                        List [Id "+", Id "x", LispNumber 1]]],
                  List [Id "dec", List [Id "lambda", List [Id "x"],
                        List [Id "-", Id "x", LispNumber 1]]]],
            List [Id "dec", List [Id "inc", LispNumber 5]]])
            `shouldBe` Right (initialEnv, LispNumber 5)

    it "letrec -> recursive function captures outer scope" $ do
        -- letrec can see outer variables too
        -- (let ([scale 2]) 
        --   (letrec ([f (lambda (n) (if (= n 0) 0 (+ scale (f (- n 1)))))])
        --     (f 3))) => 6 (2+2+2)
        eval (("scale", LispNumber 2) : initialEnv) (List [Id "letrec",
            List [List [Id "f", List [Id "lambda", List [Id "n"],
                List [Id "if", List [Id "=", Id "n", LispNumber 0],
                    LispNumber 0,
                    List [Id "+", Id "scale",
                        List [Id "f", List [Id "-", Id "n", LispNumber 1]]]]]]],
            List [Id "f", LispNumber 3]])
            `shouldBe` Right ((("scale", LispNumber 2) : initialEnv), LispNumber 6)

    it "letrec -> empty binding list" $ do
        eval initialEnv (List [Id "letrec", List [], LispNumber 42])
            `shouldBe` Right (initialEnv, LispNumber 42)

    it "letrec -> body can use all bindings" $ do
        -- (letrec ([x 1] [y 2]) (+ x y))
        eval initialEnv (List [Id "letrec",
            List [List [Id "x", LispNumber 1],
                  List [Id "y", LispNumber 2]],
            List [Id "+", Id "x", Id "y"]])
            `shouldBe` Right (initialEnv, LispNumber 3)
    
    it "define -> binds a value globally" $ do
        let expr1 = List [Id "define", Id "x", LispNumber 10]
            expr2 = Id "x"
        result1 <- eval initialEnv expr1
        case result1 of
            Right (newEnv, _) -> do
                eval newEnv expr2 `shouldBe` Right (newEnv, LispNumber 10)
            Left err -> expectationFailure $ "Define failed: " ++ err

    it "define -> allows recursive function definition (global fact)" $ do
        -- Verify that global functions can call themselves.
        -- This is distinct from 'let', which is not recursive by default.
        let factDef = List [Id "define", Id "fact", 
                            List [Id "lambda", List [Id "n"],
                                List [Id "if", List [Id "=", Id "n", LispNumber 0],
                                    LispNumber 1,
                                    List [Id "*", Id "n", List [Id "fact", List [Id "-", Id "n", LispNumber 1]]]]]]
            factCall = List [Id "fact", LispNumber 5]
            
        result1 <- eval initialEnv factDef
        case result1 of
            Right (newEnv, _) -> do
                eval newEnv factCall `shouldBe` Right (newEnv, LispNumber 120)
            Left err -> expectationFailure $ "Define failed: " ++ err

    it "define -> updates global environment (shadowing)" $ do
        -- Verify that 'define' can overwrite existing primitives or variables.
        -- We shadow the '+' operator to return 0.
        let defPlus = List [Id "define", Id "+", 
                            List [Id "lambda", List [Id "a", Id "b"], LispNumber 0]]
            addCall = List [Id "+", LispNumber 10, LispNumber 20]
            
        result1 <- eval initialEnv defPlus
        case result1 of
            Right (newEnv, _) -> do
                eval newEnv addCall `shouldBe` Right (newEnv, LispNumber 0)
            Left err -> expectationFailure $ "Define failed: " ++ err