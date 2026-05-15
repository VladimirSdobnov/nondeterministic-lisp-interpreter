module Evaluator where

import AST
import Environment
import Value

-- Вычисляет выражение
-- в заданном окружении.
eval :: Env -> Expr -> (Value, Env)

-- Числа вычисляются в самих себя
eval env (Number n) =
    (NumberV n, env)

-- Boolean вычисляются в самих себя
eval env (Boolean b) =
    (BooleanV b, env)

-- Поиск символа в окружении
eval env (Symbol s) =
    case lookupVar s env of

        Just value ->
            (value, env)

        Nothing ->
            error ("Unbound variable: " ++ s)

-- Пока списки не реализованы
eval _ (List []) =
    error "Cannot evaluate empty list"

-- Define
eval env
    (List
        [
            Symbol "define",
            Symbol name,
            valueExpr
        ]) =
    let
        (value, _) =
            eval env valueExpr

        newEnv =
            defineVar name value env
    in
        (value, newEnv)

-- Function application
eval env (List (fnExpr : argExprs)) =
    let
        -- Вычисляем функцию
        (fn, _) =
            eval env fnExpr
        -- Вычисляем аргументы
        args =
            map (fst . eval env) argExprs
    in
        apply env fn args

-- Применяет функцию к аргументам.
apply :: Env -> Value -> [Value] -> (Value, Env)

apply env (PrimitiveFunc fn) args =
    (fn args, env)

apply _ _ _ =
    error "Expected function"