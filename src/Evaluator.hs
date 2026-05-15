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

-- Последовательно вычисляет список выражений.
evalBegin :: Env -> [Expr] -> (Value, Env)

evalBegin _ [] =
    error "begin expects at least one expression"

evalBegin env [expr] =
    eval env expr

evalBegin env (expr:exprs) =
    let
        (_, newEnv) =
            eval env expr
    in
        evalBegin newEnv exprs

-- Преобразует синтаксическое выражение в runtime-значение
-- без вычисления выражения.
quoteExpr :: Expr -> Value

quoteExpr (Number n) =
    NumberV n

quoteExpr (Boolean b) =
    BooleanV b

quoteExpr (Symbol s) =
    SymbolV s

quoteExpr (List exprs) =
    ListV (map quoteExpr exprs)

-- Вычисляет логическое И с short-circuit evaluation.
evalAnd :: Env -> [Expr] -> (Value, Env)

-- Вычисляет логическое ИЛИ с short-circuit evaluation.
evalOr :: Env -> [Expr] -> (Value, Env)

evalOr env [] =
    (BooleanV False, env)

evalOr env [expr] =
    eval env expr

evalOr env (expr:exprs) =
    let
        (value, newEnv) =
            eval env expr
    in
        if isTrue value
            then
                (value, newEnv)
            else
                evalOr newEnv exprs

evalAnd env [] =
    (BooleanV True, env)

evalAnd env [expr] =
    eval env expr

evalAnd env (expr:exprs) =
    let
        (value, newEnv) =
            eval env expr
    in
        if isTrue value
            then
                evalAnd newEnv exprs
            else
                (BooleanV False, newEnv)

-- Вычисляет выражение
-- в заданном окружении.
eval :: Env -> Expr -> (Value, Env)

eval env (Number n) =
    (NumberV n, env)

eval env (Boolean b) =
    (BooleanV b, env)


eval env
    (List
        [
            Symbol "quote",
            expr
        ]) =
    (quoteExpr expr, env)

eval env
    (List
        [
            Symbol "define",
            Symbol name,
            List
                [
                    Symbol "lambda",
                    List params,
                    body
                ]
        ]) =
    let
        paramNames =
            map extractParam params
        closure =
            Closure paramNames body recursiveEnv
        recursiveEnv =
            defineVar name closure env
    in
        (closure, recursiveEnv)

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


eval env (List (Symbol "and" : exprs)) =
    evalAnd env exprs

eval env (List (Symbol "or" : exprs)) =
    evalOr env exprs

eval env (List (Symbol "begin" : exprs)) =
    evalBegin env exprs

eval env (Symbol s) =
    case lookupVar s env of
        Just value ->
            (value, env)
        Nothing ->
            error ("Unbound variable: " ++ s)

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