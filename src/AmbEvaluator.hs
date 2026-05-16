module AmbEvaluator where

import AST
import Value

-- Continuation описывает,
-- что делать с результатом вычисления.
type Continuation =
    Value -> Env -> IO ()

-- Вычисляет список аргументов слева направо.
evalArgs
    :: Env
    -> [Expr]
    -> ([Value] -> Env -> IO ())
    -> IO ()

evalArgs env [] cont =
    cont [] env

evalArgs env (expr:exprs) cont =

    eval env expr
        (\value newEnv ->
            evalArgs newEnv exprs
                (\values finalEnv ->
                    cont (value : values) finalEnv))

-- CPS evaluator.
--
-- Вместо того чтобы вернуть результат,
-- evaluator передает его в continuation.
eval :: Env -> Expr -> Continuation -> IO ()

-- Числа вычисляются в самих себя
eval env (Number n) cont =
    cont (NumberV n) env

-- Boolean вычисляются в самих себя
eval env (Boolean b) cont =
    cont (BooleanV b) env

-- Поиск символа в окружении
eval env (Symbol s) cont =
    case lookupVar s env of

        Just value ->
            cont value env

        Nothing ->
            error ("Unbound variable: " ++ s)

-- Пока списки в CPS evaluator не реализованы
eval _ (List []) _ =
    error "Cannot evaluate empty list"

-- Function application
eval env (List (fnExpr : argExprs)) cont =
    eval env fnExpr
        (\fnValue env1 ->
            evalArgs env1 argExprs
                (\argValues env2 ->
                    apply fnValue argValues cont env2))

-- Применяет функцию к аргументам.
apply
    :: Value
    -> [Value]
    -> Continuation
    -> Env
    -> IO ()

apply (PrimitiveFunc fn) args cont env =
    cont (fn args) env

apply _ _ _ _ =
    error "Expected function"