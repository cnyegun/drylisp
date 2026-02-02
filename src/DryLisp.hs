module DryLisp 
        (   Identifier
        ,   LispExpr (..)
        ) where

type Identifier = String
data LispExpr
    = LispString String
    | LispNumber Int
    | LispBool Bool
    | Id Identifier
    | List [LispExpr]
    deriving (Show, Eq)
