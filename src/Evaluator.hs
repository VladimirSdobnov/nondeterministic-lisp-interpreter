module Evaluator where

import AST
import Value

-- Проверяет, является ли значение истинным.
--
-- В Lisp только #f считается ложью.
isTrue :: Value -> Bool

isTrue (BooleanV False) =
    False

isTrue _ =
    True

-- Извлекает имя параметра функции.
extractParam :: Expr -> String

extractParam (Symbol s) =
    s

extractParam _ =
    error "Invalid parameter name"

-- Связывает параметры функции
-- с переданными аргументами.
bindParams :: [String] -> [Value] -> Env -> Env

bindParams [] [] env =
    env

bindParams (p:ps) (a:as) env =
    bindParams ps as
        (defineVar p a env)

bindParams _ _ _ =
    error "Argument count mismatch"

-- Вычисляет выражение
-- в заданном окружении.
eval :: Env -> Expr -> (Value, Env)

eval env (Number n) =
    (NumberV n, env)

eval env (Boolean b) =
    (BooleanV b, env)

eval env (Symbol s) =
    case lookupVar s env of
        Just value ->
            (value, env)
        Nothing ->
            error ("Unbound variable: " ++ s)

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

-- If
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

-- Lambda
eval env
    (List
        [
            Symbol "lambda",
            List params,
            body
        ]) =
    let
        paramNames =
            map extractParam params
    in
        (Closure paramNames body env, env)

eval _ (List []) =
    error "Cannot evaluate empty list"

-- Function application
eval env (List (fnExpr : argExprs)) =
    let
        (fn, _) =
            eval env fnExpr
        args =
            map (fst . eval env) argExprs
    in
        apply env fn args

-- Применяет функцию к аргументам.
apply :: Env -> Value -> [Value] -> (Value, Env)

apply env (PrimitiveFunc fn) args =
    (fn args, env)

apply _ (Closure params body closureEnv) args =
    let
        newEnv =
            bindParams params args closureEnv
    in
        eval newEnv body

apply _ _ _ =
    error "Expected function"