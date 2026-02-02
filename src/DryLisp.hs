module DryLisp 
        (   Identifier
        ,   LispExpr (..)
        ) where
import Data.Scientific (Scientific)

type Identifier = String
data LispExpr
    = LispString String
    | LispNumber Scientific
    | LispBool Bool
    | Id Identifier
    | List [LispExpr]
    deriving (Show, Eq)
