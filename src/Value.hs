module Value where

import qualified Data.Map as Map
import AST

-- Таблица переменных интерпретатора.
--
-- String  -> имя переменной
-- Value   -> значение переменной
type Env = Map.Map String Value

-- Создает пустое окружение.
emptyEnv :: Env
emptyEnv = Map.empty

-- Ищет значение переменной в окружении.
lookupVar :: String -> Env -> Maybe Value
lookupVar = Map.lookup

-- Добавляет переменную в окружение.
defineVar :: String -> Value -> Env -> Env
defineVar = Map.insert

-- Runtime-значения интерпретатора.
data Value
    = NumberV Integer
    | BooleanV Bool
    | SymbolV String
    | ListV [Value]
    | PrimitiveFunc ([Value] -> Value)
    | Closure [String] Expr Env

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

    show (Closure {}) =
        "<closure>"

instance Eq Value where

    NumberV a == NumberV b =
        a == b

    BooleanV a == BooleanV b =
        a == b

    SymbolV a == SymbolV b =
        a == b

    ListV a == ListV b =
        a == b

    PrimitiveFunc _ == PrimitiveFunc _ =
        False
        
    Closure {} == Closure {} =
        False

    _ == _ =
        False
