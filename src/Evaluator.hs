module Evaluator where

import AST
import Environment
import Value

-- Проверяет, является ли значение истинным.
--
-- В Lisp только #f считается ложью.
isTrue :: Value -> Bool

isTrue (BooleanV False) =
    False

isTrue _ =
    True

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

-- Define
--
-- Создает новое связывание в окружении.
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

-- If
--
-- Вычисляет только одну из двух веток
-- в зависимости от условия.
eval env
    (List
        [
            Symbol "if",
            conditionExpr,
            thenExpr,
            elseExpr
        ]) =

    let

        (conditionValue, env1) =
            eval env conditionExpr

    in

        if isTrue conditionValue
            then
                eval env1 thenExpr

            else
                eval env1 elseExpr

-- Пустой список нельзя вычислить
eval _ (List []) =
    error "Cannot evaluate empty list"

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