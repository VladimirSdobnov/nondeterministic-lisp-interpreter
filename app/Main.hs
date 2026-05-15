module Main where

import AST
import Environment
import Evaluator
import Primitives
import Value

main :: IO ()
main = do

    putStrLn "================================="
    putStrLn "Evaluator tests"
    putStrLn "================================="

    -- Number
    print
        (eval primitiveEnv (Number 42))

    -- Boolean
    print
        (eval primitiveEnv (Boolean True))

    -- Variable lookup
    let env =
            defineVar "x" (NumberV 100) primitiveEnv

    print
        (eval env (Symbol "x"))