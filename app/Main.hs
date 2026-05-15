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

    putStrLn "========== QUOTE TESTS =========="

    -- Обычный список данных
    _ <-
        runProgram primitiveEnv
            "(quote (1 2 3 4 5))"

    -- Код как данные
    _ <-
        runProgram primitiveEnv
            "(quote (+ 1 2))"

    -- Вложенные списки
    _ <-
        runProgram primitiveEnv
            "(quote ((1 2) (3 4) (5 6)))"

    -- Символы внутри quoted структуры
    _ <-
        runProgram primitiveEnv
            "(quote (define x (+ 1 2)))"

    -- Сравнение quote и list
    --
    -- Здесь (+ 1 2) вычисляется
    _ <-
        runProgram primitiveEnv
            "(list (+ 1 2) (* 2 3))"

    -- А здесь НЕ вычисляется
    _ <-
        runProgram primitiveEnv
            "(quote ((+ 1 2) (* 2 3)))"

    putStrLn "========== SYMBOLIC PROGRAM TEST =========="

    -- Пример символической программы
    --
    -- Здесь мы фактически храним код как данные
    _ <-
        runProgram primitiveEnv
            (concat
                [
                    "(quote ",

                        "(begin ",

                            "(define factorial ",
                                "(lambda (n) ",
                                    "(if (= n 0) ",
                                        "1 ",
                                        "(* n (factorial (- n 1)))))) ",

                            "(factorial 5)))"
                ])

    return ()