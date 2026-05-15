module Main where

import Evaluator
import Parser
import Primitives
import Value

runProgram :: Env -> String -> IO Env
runProgram env input = do

    putStrLn "================================="
    putStrLn ("Input: " ++ input)

    let tokens =
            tokenize input

    let expr =
            parse tokens

    let (result, newEnv) =
            eval env expr

    putStrLn ("Result: " ++ show result)

    return newEnv

main :: IO ()
main = do

    putStrLn ""
    putStrLn "========== LAMBDA TESTS =========="

    -- Простая lambda
    env1 <-
        runProgram primitiveEnv
            "((lambda (x) (+ x 1)) 5)"

    -- Lambda с несколькими аргументами
    env2 <-
        runProgram env1
            "((lambda (x y) (+ (* x 2) (* y 3))) 4 5)"

    -- Lambda внутри lambda
    env3 <-
        runProgram env2
            "((lambda (x) ((lambda (y) (+ x y)) 7)) 10)"

    -- Define пользовательской функции
    env4 <-
        runProgram env3
            "(define square (lambda (x) (* x x)))"

    -- Использование пользовательской функции
    env5 <-
        runProgram env4
            "(square 6)"

    putStrLn ""
    putStrLn "========== IF + LAMBDA TEST =========="

    -- В зависимости от результата первой функции
    -- выбирается вторая или третья
    --
    -- Если (> 10 5) -> вызывается первая lambda
    -- Иначе -> вторая lambda
    env6 <-
        runProgram env5
            "((if (> 10 5) (lambda (x) (* x 2)) (lambda (x) (+ x 100))) 7)"

    putStrLn ""
    putStrLn "========== COMPLEX TESTS =========="

    -- Сложный арифметический pipeline
    env7 <-
        runProgram env6
            "((lambda (a b c) (+ (* a b) (* c 10))) 2 5 3)"

    -- Nested if + lambda
    env8 <-
        runProgram env7
            "((lambda (x) (if (> x 10) (* x 2) (+ x 100))) 15)"

    -- Closure test
    --
    -- Внутренняя lambda использует x
    -- из внешнего окружения
    _ <-
        runProgram env8
            "(((lambda (x) (lambda (y) (+ x y))) 10) 5)"

    return ()