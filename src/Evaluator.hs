module Evaluator where

import AST
import Environment
import Value

-- Вычисляет выражение
-- в заданном окружении.
eval :: Env -> Expr -> Value

-- Числа вычисляются в самих себя
eval _ (Number n) =
    NumberV n

-- Boolean вычисляются в самих себя
eval _ (Boolean b) =
    BooleanV b

-- Поиск символа в окружении
eval env (Symbol s) =
    case lookupVar s env of

        Just value ->
            value

        Nothing ->
            error ("Unbound variable: " ++ s)

-- Пока списки не реализованы
eval _ (List []) =
    error "Cannot evaluate empty list"

eval _ (List _) =
    error "List evaluation not implemented yet"