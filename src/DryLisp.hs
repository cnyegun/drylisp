{-# LANGUAGE LambdaCase #-}
module DryLisp
    (   Identifier
    ,   Env
    ,   LispExpr (..)
    ,   eval
    ,   initialEnv
    ) where
import Control.Monad

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
            , closureFn :: [LispExpr] -> Either ErrorMsg (Env, LispExpr) }

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
    LispClosure x_env _ == LispClosure y_env _ = x_env == y_env
    _ == _ = False

eval :: Env -> LispExpr -> Either ErrorMsg (Env, LispExpr)

-- Atomic -> Evalue to itself
eval env (LispString s) = Right (env, LispString s)
eval env (LispBool b) = Right (env, LispBool b)
eval env (LispNumber n) = Right (env, LispNumber n)
eval env (List []) = Right (env, List [])

-- Id -> Lookup in Env
eval [] (Id name) = Left ("Unbound variable: " ++ name)
eval ((key, value):rest) (Id name) =
    if name == key then Right ((key, value):rest, value)
    else eval rest (Id name)

-- Quote -> Return expression unevaluated
eval env (List [Id "quote", rest]) = Right (env, rest)

-- If else then
eval env (List [Id "if", condExpr, thenExpr, elseExpr]) = do
    (newEnv, condVal) <- eval env condExpr
    case condVal of
        LispBool False -> eval newEnv elseExpr
        _ -> eval newEnv thenExpr

eval _ (List (Id "if": _)) = Left "if requires exactly 3 arguments"

-- Lambda -> create a closure and zip the params (string) to the args (LispExpr) 
--           then append it to the env 
eval env (List [Id "lambda", List params, body]) =
    Right (env, LispClosure env $ \args -> do
        paramNames <- mapM (\case (Id name) -> Right name; _ -> Left "lambda: invalid parameter") params
        if length paramNames /= length args
            then Left "lambda: arity mismatch"
        else
            let newEnv =  zip paramNames args ++ env
            in eval newEnv body)

eval env (List [Id "let", List bindings, body]) = do
    binds <- mapM extractBinding bindings
    lispLet env binds body

-- sequential let
eval env (List [Id "let*", List bindings, body]) = do
    binds <- mapM extractBinding bindings
    lispSeqLet env binds body

-- recursive let
eval env (List [Id "letrec", List bindings, body]) = do
    binds <- mapM extractBinding bindings
    lispLetrec env binds body

eval env (List [Id "define", Id name, body]) = 
    let names = [name]
        env' = zip names vals ++ env
        exprs = [body]
        vals = map (\e -> case eval env' e of
            Right (_, v) -> v
            Left msg -> error msg
            ) exprs
    in do
        Right (env', Id name)

eval env (List (Id "define" : List (Id name : params) : body)) = 
    let body' = case body of 
            [single] -> single
            multiple -> List (Id "begin": multiple)
    in eval env (List [Id "define", Id name, List [Id "lambda", List params, body']])

eval _ (List [Id "define", _, _]) = Left "define: syntax error"

eval env (List (Id "begin": body)) = 
    case body of 
        [] -> Left "begin: empty sequence"
        _ -> do 
            results <- mapM (eval env) body
            return (env, snd $ last results)

eval env (List (f : args)) = do
    (_, fnClosure) <- eval env f
    argsValWithEnv <- mapM (eval env) args
    let argsVal = map snd argsValWithEnv
    result <- apply fnClosure argsVal
    Right (env, result)

-- Fall through
eval _ _ = Left "Unknown expression"

extractBinding :: LispExpr -> Either ErrorMsg (Identifier, LispExpr)
extractBinding (List [Id name, expr]) = Right (name, expr)
extractBinding _ = Left "Invalid let binding syntax"

apply :: LispExpr -> [LispExpr] -> Either ErrorMsg LispExpr
apply (LispClosure _ fn) args =
    case fn args of
        Right (_, val) -> Right val
        Left msg -> Left msg

apply _ _ = Left "Not a function"

-- $ =============== $
--      primitives
-- $ =============== $

lispAdd :: LispExpr
lispAdd = LispClosure [] $ \args -> do
    nums <- mapM (\case LispNumber n -> Right n; _ -> Left "+ requires numbers") args
    Right ([], LispNumber (sum nums))

lispSub :: LispExpr
lispSub = LispClosure [] $ \case
    [LispNumber a, LispNumber b] -> Right ([], LispNumber (a - b))
    _ -> Left "- requires exactly 2 numbers"

lispMul :: LispExpr
lispMul = LispClosure [] $ \args -> do
    nums <- mapM (\case LispNumber n -> Right n; _ -> Left "* requires numbers") args
    Right ([], LispNumber (product nums))

lispCons :: LispExpr
lispCons = LispClosure [] $ \case
    [x, List xs] -> Right ([], List (x:xs))
    _ -> Left "cons requires the second argument must be a list"

lispCar :: LispExpr
lispCar = LispClosure [] $ \case
    [List (x:_)] -> Right ([], x)
    [List []]    -> Left "car requires a non-empty list"
    _ -> Left "car requires a list"

lispCdr :: LispExpr
lispCdr = LispClosure [] $ \case
    [List (_:xs)] -> Right ([], List xs)
    _ -> Left "cdr requires a non-empty list"

-- Let's make it strict (like Scheme and Common Lisp) YEAH!
lispEq :: LispExpr
lispEq = LispClosure [] $ \case
    [LispNumber a, LispNumber b] -> Right ([], LispBool (a == b))
    [LispString a, LispString b] -> Right ([], LispBool (a == b))
    [LispBool   a, LispBool   b] -> Right ([], LispBool (a == b))
    _                            -> Left "Wrong type for comparison"

lispEmpty :: LispExpr
lispEmpty = LispClosure [] $ \case
    [List (_:_)] -> Right ([], LispBool False)
    [List []]     -> Right ([], LispBool True)
    _             -> Left "Not a list"

lispGt :: LispExpr
lispGt = LispClosure [] $ \case
    [LispNumber a, LispNumber b] -> Right ([], LispBool (a > b))
    _                            -> Left "> requires exactly 2 numbers"

lispGe :: LispExpr
lispGe = LispClosure [] $ \case
    [LispNumber a, LispNumber b] -> Right ([], LispBool (a >= b))
    _                            -> Left ">= requires exactly 2 numbers"

lispLt :: LispExpr
lispLt = LispClosure [] $ \case
    [LispNumber a, LispNumber b] -> Right ([], LispBool (a < b))
    _                            -> Left "< requires exactly 2 numbers"

lispLe :: LispExpr
lispLe = LispClosure [] $ \case
    [LispNumber a, LispNumber b] -> Right ([], LispBool (a <= b))
    _                            -> Left "<= requires exactly 2 numbers"

lispLet :: Env -> [(Identifier, LispExpr)] -> LispExpr -> Either ErrorMsg (Env, LispExpr)
lispLet env bindings body = do
    -- Extract the bindings tuples
    -- Evaluate each binding's LispExpr
    -- Zip it again :D
    let (names, exprs) = unzip bindings
    exprValWithEnv <- mapM (eval env) exprs
    let exprVal = map snd exprValWithEnv
    let newEnv = zip names exprVal ++ env
    (_, result) <- eval newEnv body
    Right (env, result)

lispSeqLet :: Env -> [(Identifier, LispExpr)] -> LispExpr -> Either ErrorMsg (Env, LispExpr)
lispSeqLet env ((name, expr):rest) body = do
    (_, val) <- eval env expr
    (_, result) <- lispSeqLet ((name, val):env) rest body
    Right (env, result)
lispSeqLet env [] body = eval env body

lispLetrec :: Env -> [(Identifier, LispExpr)] -> LispExpr -> Either ErrorMsg (Env, LispExpr)
lispLetrec env bindings body = do
    let (names, expressions) = unzip bindings
        env' = zip names vals ++ env
        vals = map (\e -> case eval env' e of
            Right (_, v) -> v
            Left msg     -> error msg
            ) expressions
    (_, result) <- eval env' body
    Right (env, result)

-- $ =============== $
--     initial env
-- $ =============== $

initialEnv :: Env
initialEnv = [ ("+", lispAdd), ("-", lispSub), ("*", lispMul)
             , ("cons", lispCons), ("car", lispCar), ("cdr", lispCdr)
             , ("=", lispEq), ("empty?", lispEmpty), ("null?", lispEmpty)
             , (">", lispGt), (">=", lispGe), ("<", lispLt), ("<=", lispLe)
             , ("map", lispMap), ("filter", lispFilter), ("fold", lispFold)
             , ("length", lispLength), ("append", lispAppend), ("reverse", lispReverse)
             ]

-- $ =============== $
--     higher order
-- $ =============== $

lispMap :: LispExpr
lispMap = LispClosure [] $ \case
    [fn, List xs] -> do
        ys <- mapM (apply fn . pure) xs
        Right ([], List ys)
    _ -> Left "map requires a function and a list"

lispFilter :: LispExpr
lispFilter = LispClosure [] $ \case
    [fn, List xs] -> do
        ys <- filterM (\x -> do
            res <- apply fn [x]
            case res of
                LispBool False -> return False
                _              -> return True) xs
        Right ([], List ys)
    _ -> Left "filter requires a function and a list"

lispFold :: LispExpr  -- left fold (reduce)
lispFold = LispClosure [] $ \case
    [fn, initial, List xs] -> do
        acc <- foldM (\a x -> apply fn [a, x]) initial xs
        Right ([], acc)
    _ -> Left "fold requires function, initial value, and list"

lispLength :: LispExpr
lispLength = LispClosure [] $ \case
    [List xs] -> Right ([], LispNumber (fromIntegral $ length xs))
    _         -> Left "length requires a list"

lispAppend :: LispExpr
lispAppend = LispClosure [] $ \args -> do
    lists <- mapM (\case List xs -> Right xs; _ -> Left "append requires lists") args
    Right ([], List $ concat lists)

lispReverse :: LispExpr
lispReverse = LispClosure [] $ \case
    [List xs] -> Right ([], List $ reverse xs)
    _         -> Left "reverse requires a list"