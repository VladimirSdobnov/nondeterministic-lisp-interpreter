module Main where

import Evaluator
import Parser
import Primitives
import Value

runProgram :: Env -> String -> IO Env
runProgram env input = do

    putStrLn "================================="
    putStrLn "Program:"
    putStrLn input

    let tokens =
            tokenize input

    let expr =
            parse tokens

    let (result, newEnv) =
            eval env expr

    putStrLn ""
    putStrLn ("Result: " ++ show result)
    putStrLn ""

    return newEnv

main :: IO ()
main = do

    putStrLn "========== NOT TESTS =========="

    _ <-
        runProgram primitiveEnv
            "(not #t)"

    _ <-
        runProgram primitiveEnv
            "(not #f)"

    _ <-
        runProgram primitiveEnv
            "(not 42)"

    putStrLn "========== AND TESTS =========="

    -- Все true
    _ <-
        runProgram primitiveEnv
            "(and #t #t #t)"

    -- Встречается false
    _ <-
        runProgram primitiveEnv
            "(and #t #f #t)"

    -- Short-circuit test
    --
    -- Деление не должно вычисляться
    _ <-
        runProgram primitiveEnv
            "(and #f (/ 1 0))"

    -- Сложное логическое выражение
    _ <-
        runProgram primitiveEnv
            "(and (> 10 5) (< 2 3) (= 4 4))"

    putStrLn "========== OR TESTS =========="

    -- Первый true
    _ <-
        runProgram primitiveEnv
            "(or #t #f #f)"

    -- Последний true
    _ <-
        runProgram primitiveEnv
            "(or #f #f (> 10 5))"

    -- Все false
    _ <-
        runProgram primitiveEnv
            "(or #f #f #f)"

    -- Short-circuit test
    --
    -- Деление не должно вычисляться
    _ <-
        runProgram primitiveEnv
            "(or #t (/ 1 0))"

    putStrLn "========== COMPLEX BOOLEAN PROGRAM =========="

    -- Более содержательный пример
    --
    -- Проверяем число по нескольким условиям
    _ <-
        runProgram primitiveEnv
            (concat
                [
                    "(begin ",

                    "(define classify ",
                        "(lambda (x) ",
                            "(if (and (> x 0) (< x 10)) ",
                                "100 ",
                                "(if (or (= x 0) (= x 10)) ",
                                    "200 ",
                                    "300)))) ",

                    "(+ (classify 5) ",
                       "(classify 0) ",
                       "(classify 20)))"
                ])

    return ()