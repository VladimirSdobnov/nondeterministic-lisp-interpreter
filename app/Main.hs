module Main where

import AmbEvaluator
import AST
import Parser
import Primitives
import Value

-- ============================================================
-- Continuations
-- ============================================================

-- Обычный continuation.
--
-- Просто печатает результат.
printSuccess :: SuccessCont
printSuccess value _ _ =
    putStrLn ("Result: " ++ show value)

-- Failure continuation.
printFailure :: FailureCont
printFailure =
    putStrLn "Computation failed"

-- Continuation,
-- дополнительно преобразующий результат.
--
-- Демонстрирует,
-- что continuation может продолжать вычисления.
doubleNumberCont :: SuccessCont
doubleNumberCont (NumberV n) _ _ =
    putStrLn ("Doubled result: " ++ show (n * 2))

doubleNumberCont value _ _ =
    putStrLn ("Expected number, got: " ++ show value)

-- Continuation,
-- выводящий тип результата.
describeCont :: SuccessCont
describeCont (NumberV n) _ _ =
    putStrLn ("Number result: " ++ show n)

describeCont (BooleanV b) _ _ =
    putStrLn ("Boolean result: " ++ show b)

describeCont (ListV values) _ _ =
    putStrLn ("List result with "
        ++ show (length values)
        ++ " elements")

describeCont value _ _ =
    putStrLn ("Other result: " ++ show value)

-- ============================================================
-- Runner
-- ============================================================

runProgram
    :: String
    -> SuccessCont
    -> IO ()

runProgram input success = do

    putStrLn "================================="
    putStrLn ("Program: " ++ input)

    let tokens =
            tokenize input

    let expr =
            parse tokens

    eval
        primitiveEnv
        expr
        success
        printFailure

    putStrLn ""

-- ============================================================
-- Main
-- ============================================================

main :: IO ()
main = do

    putStrLn "========== STANDARD CONTINUATION =========="

    runProgram
        "(+ 1 2)"
        printSuccess

    runProgram
        "(* 2 3 4)"
        printSuccess

    putStrLn "========== CUSTOM CONTINUATION =========="

    runProgram
        "(+ 10 20)"
        doubleNumberCont

    runProgram
        "(* 5 5)"
        doubleNumberCont

    putStrLn "========== DESCRIBE CONTINUATION =========="

    runProgram
        "(list 1 2 3 4)"
        describeCont

    runProgram
        "(> 10 5)"
        describeCont

    runProgram
        "(+ 100 200)"
        describeCont

    putStrLn "========== RECURSIVE PROGRAM =========="

    runProgram
        (concat
            [
                "(begin ",

                "(define fact ",
                    "(lambda (n) ",
                        "(if (= n 0) ",
                            "1 ",
                            "(* n (fact (- n 1)))))) ",

                "(fact 6))"
            ])
        printSuccess