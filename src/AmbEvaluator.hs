module AmbEvaluator where

import AST
import Value
import Prelude hiding (fail)

-- Success continuation.
--
-- Получает:
-- 1. вычисленное значение
-- 2. новое окружение
-- 3. continuation для следующей альтернативы
type SuccessCont =
    Value -> Env -> FailureCont -> IO ()

-- Failure continuation.
--
-- Вызывается,
-- когда текущая ветка вычисления провалилась.
type FailureCont =
    IO ()

-- Проверяет значение на истинность.
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
-- с аргументами.
bindParams :: [String] -> [Value] -> Env -> Env

bindParams [] [] env =
    env

bindParams (p:ps) (a:as) env =
    bindParams ps as
        (defineVar p a env)

bindParams _ _ _ =
    error "Argument count mismatch"

-- Вычисляет список аргументов слева направо.
evalArgs :: Env
    -> [Expr]
    -> ([Value] -> Env -> FailureCont -> IO ())
    -> FailureCont
    -> IO ()

evalArgs env [] success fail =
    success [] env fail

evalArgs env (expr:exprs) success fail =
    eval env expr
        (\value newEnv fail1 ->
            evalArgs newEnv exprs
                (\values finalEnv fail2 ->
                    success
                        (value : values)
                        finalEnv
                        fail2)
                fail1)
        fail

-- Последовательно вычисляет список выражений.
evalBegin :: Env
    -> [Expr]
    -> SuccessCont
    -> FailureCont
    -> IO ()

evalBegin _ [] _ fail =
    fail

-- Последнее выражение
evalBegin env [expr] success fail =
    eval env expr success fail

-- Промежуточные выражения
evalBegin env (expr:exprs) success fail =
    eval env expr
        (\_ newEnv fail1 ->
            evalBegin
                newEnv
                exprs
                success
                fail1)
        fail

-- CPS evaluation для and.
evalAnd :: Env
    -> [Expr]
    -> SuccessCont
    -> FailureCont
    -> IO ()

-- Пустой and -> #t
evalAnd env [] success fail =
    success (BooleanV True) env fail

-- Последнее выражение
evalAnd env [expr] success fail =
    eval env expr success fail

-- Short-circuit logic
evalAnd env (expr:exprs) success fail =
    eval env expr
        (\value env1 fail1 ->
            if isTrue value
                then
                    evalAnd env1 exprs success fail1
                else
                    success (BooleanV False) env1 fail1)
        fail

-- CPS evaluation для or.
evalOr :: Env
    -> [Expr]
    -> SuccessCont
    -> FailureCont
    -> IO ()

-- Пустой or -> #f
evalOr env [] success fail =
    success (BooleanV False) env fail

-- Последнее выражение
evalOr env [expr] success fail =
    eval env expr success fail

-- Short-circuit logic
evalOr env (expr:exprs) success fail =

    eval env expr
        (\value env1 fail1 ->
            if isTrue value
                then
                    success value env1 fail1
                else
                    evalOr env1 exprs success fail1)
        fail

-- Вычисляет альтернативы amb.
evalAmb :: Env
    -> [Expr]
    -> SuccessCont
    -> FailureCont
    -> IO ()

evalAmb _ [] _ fail =
    fail

-- Пробуем первую альтернативу.
-- Если она потом провалится,
-- пробуем остальные альтернативы.
evalAmb env (choice:choices) success fail =
    eval env choice
        success
        (evalAmb env choices success fail)

-- Преобразует Expr в Value
-- без вычисления.
quoteExpr :: Expr -> Value

quoteExpr (Number n) =
    NumberV n

quoteExpr (Boolean b) =
    BooleanV b

quoteExpr (Symbol s) =
    SymbolV s

quoteExpr (List exprs) =
    ListV (map quoteExpr exprs)

-- CPS evaluator.
--
-- Вместо того чтобы вернуть результат,
-- evaluator передает его в continuation.
eval :: Env
    -> Expr
    -> SuccessCont
    -> FailureCont
    -> IO ()

-- Числа вычисляются в самих себя
eval env (Number n) success fail =
    success (NumberV n) env fail

-- Boolean вычисляются в самих себя
eval env (Boolean b) success fail =
    success (BooleanV b) env fail

-- Поиск символа в окружении
eval env (Symbol s) success fail =
    case lookupVar s env of
        Just value ->
            success value env fail
        Nothing ->
            error ("Unbound variable: " ++ s)

-- Пока списки в CPS evaluator не реализованы
eval _ (List []) _ _ =
    error "Cannot evaluate empty list"

-- And
eval env (List (Symbol "and" : exprs)) success fail =
    evalAnd env exprs success fail

-- Or
eval env (List (Symbol "or" : exprs)) success fail =
    evalOr env exprs success fail

-- If
eval env
    (List
        [
            Symbol "if",
            condition,
            trueBranch,
            falseBranch
        ])
    success
    fail =
    eval env condition
        (\conditionValue env1 fail1 ->
            if isTrue conditionValue
                then
                    eval env1 trueBranch success fail1
                else
                    eval env1 falseBranch success fail1)
        fail

-- Lambda
eval env
    (List
        [
            Symbol "lambda",
            List params,
            body
        ])
    success
    fail =
    let
        paramNames =
            map extractParam params
        closure =
            Closure paramNames body env
    in
        success closure env fail

-- Define recursive function
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
        ])
    success
    fail =
    let
        paramNames =
            map extractParam params
        closure =
            Closure paramNames body recursiveEnv
        recursiveEnv =
            defineVar name closure env
    in
        success closure recursiveEnv fail

-- Define variable
eval env
    (List
        [
            Symbol "define",
            Symbol name,
            valueExpr
        ])
    success
    fail =
    eval env valueExpr
        (\value env1 fail1 ->
            let
                newEnv =
                    defineVar name value env1
            in
                success value newEnv fail1)
        fail

-- Quote
eval env
    (List
        [
            Symbol "quote",
            expr
        ])
    success
    fail =
    success
        (quoteExpr expr)
        env
        fail

-- Amb
eval env (List (Symbol "amb" : choices)) success fail =
    evalAmb env choices success fail

-- Require
eval env
    (List
        [
            Symbol "require",
            condition
        ])
    success
    fail =
    eval env condition
        (\value env1 fail1 ->
            if isTrue value
                then
                    success (BooleanV True) env1 fail1
                else
                    fail1)
        fail

-- Begin
eval env (List (Symbol "begin" : exprs)) success fail =
    evalBegin env exprs success fail

-- Function application
eval env (List (fnExpr : argExprs)) success fail =
    eval env fnExpr
        (\fnValue env1 fail1 ->
            evalArgs env1 argExprs
                (\argValues env2 fail2 ->
                    apply
                        fnValue
                        argValues
                        success
                        fail2
                        env2)
                fail1)
        fail

-- Применяет функцию к аргументам.
apply :: Value
    -> [Value]
    -> SuccessCont
    -> FailureCont
    -> Env
    -> IO ()

-- Primitive functions
apply (PrimitiveFunc fn) args success fail env =
    success (fn args) env fail

-- User-defined functions
apply (Closure params body closureEnv)
      args
      success
      fail
      _ =
    let
        newEnv =
            bindParams params args closureEnv
    in
        eval newEnv body success fail

apply _ _ _ _ _ =
    error "Expected function"