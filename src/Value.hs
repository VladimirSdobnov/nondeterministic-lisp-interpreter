module Value where

import AST

-- Значения, которые существуют во время выполнения программы.
data Value
    = NumberV Integer
    | BooleanV Bool
    | SymbolV String
    | ListV [Value]
    | PrimitiveFunc ([Value] -> Value)
    deriving (Show)

-- Преобразует runtime-значение обратно в строку.
showValue :: Value -> String
showValue (NumberV n) =
    show n

showValue (BooleanV True) =
    "#t"

showValue (BooleanV False) =
    "#f"

showValue (SymbolV s) =
    s

showValue (ListV values) =
    "(" ++ unwords (map showValue values) ++ ")"

showValue (PrimitiveFunc _) =
    "<primitive>"