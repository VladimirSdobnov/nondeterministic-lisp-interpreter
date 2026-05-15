module Value where

-- Runtime-значения интерпретатора.
data Value
    = NumberV Integer
    | BooleanV Bool
    | SymbolV String
    | ListV [Value]
    | PrimitiveFunc ([Value] -> Value)

-- Пользовательское отображение runtime-значений.
instance Show Value where

    show (NumberV n) =
        show n

    show (BooleanV True) =
        "#t"

    show (BooleanV False) =
        "#f"

    show (SymbolV s) =
        s

    show (ListV values) =
        "(" ++ unwords (map show values) ++ ")"

    show (PrimitiveFunc _) =
        "<primitive>"