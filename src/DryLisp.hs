{-# LANGUAGE LambdaCase #-}
module DryLisp 
    (   Identifier
    ,   LispExpr (..)
    ,   eval       
    ,   initialEnv
    ) where

type Identifier = String
type Env = [(String, LispExpr)]
type ErrorMsg = String
data LispExpr
    = LispString String
    | LispNumber Double
    | LispBool Bool
    | Id Identifier
    | List [LispExpr]
    | LispClosure 
            { closureEnv :: Env
            , closureFn :: [LispExpr] -> Either ErrorMsg LispExpr }

instance Show LispExpr where
    show (LispNumber n) = show n
    show (LispString s) = show s
    show (LispBool b) = if b then "#t" else "#f"
    show (Id name) = name
    show (List xs) = "(" ++ unwords (map show xs) ++ ")"
    show (LispClosure _ _) = "<closure>"

instance Eq LispExpr where
    LispString s1 == LispString s2 = s1 == s2
    LispNumber n1 == LispNumber n2 = n1 == n2
    LispBool b1   == LispBool b2   = b1 == b2
    Id i1         == Id i2         = i1 == i2
    List xs1      == List xs2      = xs1 == xs2
    _ == _ = False 

eval :: Env -> LispExpr -> Either ErrorMsg LispExpr

-- Atomic -> Evalue to itself
eval _ (LispString s) = Right (LispString s)
eval _ (LispBool b) = Right (LispBool b)
eval _ (LispNumber n) = Right (LispNumber n)

-- Id -> Lookup in Env
eval [] (Id name) = Left ("Unbound variable: " ++ name)
eval ((key, value):rest) (Id name) = 
    if name == key then Right value
    else eval rest (Id name)

-- Quote -> Return expression unevaluated
eval _ (List [Id "quote", rest]) = Right rest

-- If else then
eval env (List [Id "if", condExpr, thenExpr, elseExpr]) = do
    condVal <- eval env condExpr
    case condVal of 
        LispBool False -> eval env elseExpr
        _ -> eval env thenExpr

eval _ (List (Id "if": _)) = Left "if requires exactly 3 arguments"


eval env (List [Id "lambda", List params, body]) =
    Right $ LispClosure env $ \args -> do
        paramNames <- mapM (\case (Id name) -> Right name; _ -> Left "lambda: failed") params
        if length paramNames /= length args 
            then Left "lambda: arity mismatch"
        else 
            let newEnv = zip paramNames args ++ env
            in eval newEnv body

eval env (List (f : args)) = do
    fnClosure <- eval env f
    argsVal <- mapM (eval env) args
    apply fnClosure argsVal

-- Fall through
eval _ (List []) = Left "Empty application"
eval _ _ = Left "Unknown expression"

apply :: LispExpr -> [LispExpr] -> Either ErrorMsg LispExpr
apply (LispClosure _ fn) args = fn args
apply _ _ = Left "Not a function"

-- $ =============== $
--      primitives
-- $ =============== $

lispAdd :: LispExpr
lispAdd = LispClosure [] $ \args -> do
    nums <- mapM (\case LispNumber n -> Right n; _ -> Left "+ requires numbers") args
    Right (LispNumber (sum nums))

lispSub :: LispExpr
lispSub = LispClosure [] $ \case
    [LispNumber a, LispNumber b] -> Right (LispNumber (a - b))
    _ -> Left "- requires exactly 2 numbers"

lispMul :: LispExpr
lispMul = LispClosure [] $ \args -> do
    nums <- mapM (\case LispNumber n -> Right n; _ -> Left "* requires numbers") args
    Right (LispNumber (product nums))

lispCons :: LispExpr
lispCons = LispClosure [] $ \case
    [x, List xs] -> Right (List (x:xs))
    _ -> Left "cons requires the second argument must be a list"

lispCar :: LispExpr
lispCar = LispClosure [] $ \case
    [List (x:_)] -> Right x
    [List []]    -> Left "car requires a non-empty list"
    _ -> Left "car requires a list"

lispCdr :: LispExpr
lispCdr = LispClosure [] $ \case
    [List (_:xs)] -> Right (List xs)
    _ -> Left "cdr requires a non-empty list"

-- Let's make it strict (like Scheme and Common Lisp) YEAH!
lispEq :: LispExpr
lispEq = LispClosure [] $ \case
    [LispNumber a, LispNumber b] -> Right (LispBool (a == b))
    [LispString a, LispString b] -> Right (LispBool (a == b))
    [LispBool   a, LispBool   b] -> Right (LispBool (a == b))
    _                            -> Left "Wrong type for comparison" 
    
lispEmpty :: LispExpr
lispEmpty = LispClosure [] $ \case
    [List (_:_)] -> Right (LispBool False)
    [List []]     -> Right (LispBool True)
    _             -> Left "Not a list"

lispGt :: LispExpr
lispGt = LispClosure [] $ \case
    [LispNumber a, LispNumber b] -> Right (LispBool (a > b))
    _                            -> Left "> requires exactly 2 numbers"

lispGe :: LispExpr
lispGe = LispClosure [] $ \case
    [LispNumber a, LispNumber b] -> Right (LispBool (a >= b))
    _                            -> Left ">= requires exactly 2 numbers"

lispLt :: LispExpr
lispLt = LispClosure [] $ \case
    [LispNumber a, LispNumber b] -> Right (LispBool (a < b))
    _                            -> Left "< requires exactly 2 numbers"

lispLe :: LispExpr
lispLe = LispClosure [] $ \case
    [LispNumber a, LispNumber b] -> Right (LispBool (a <= b))
    _                            -> Left "<= requires exactly 2 numbers"

-- $ =============== $
--     initial env
-- $ =============== $

initialEnv :: Env
initialEnv = [ ("+", lispAdd)
             , ("-", lispSub)
             , ("*", lispMul)
             , ("cons", lispCons)
             , ("car", lispCar)
             , ("cdr", lispCdr)
             , ("=", lispEq)
             , ("empty?", lispEmpty)
             , ("null?", lispEmpty)
             , (">", lispGt)
             , (">=", lispGe)
             , ("<", lispLt)
             , ("<=", lispLe)
             ]