module AST where

data Expr
    = Number Integer
    | Boolean Bool
    | Symbol String
    | List [Expr]
    deriving (Eq, Show)