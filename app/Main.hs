module Main where

import Environment
import Evaluator
import Parser
import Primitives

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
    putStrLn "========== DEFINE TESTS =========="

    env1 <-
        runProgram primitiveEnv
            "(define x 10)"

    env2 <-
        runProgram env1
            "(define y 5)"

    putStrLn ""
    putStrLn "========== VARIABLE TESTS =========="

    env3 <-
        runProgram env2
            "(+ x y)"

    env4 <-
        runProgram env3
            "(* x y)"

    putStrLn ""
    putStrLn "========== IF TESTS =========="

    env5 <-
        runProgram env4
            "(if #t 100 200)"

    env6 <-
        runProgram env5
            "(if #f 100 200)"

    putStrLn ""
    putStrLn "========== COMPARISON TESTS =========="

    env7 <-
        runProgram env6
            "(if (> x y) 1 0)"

    env8 <-
        runProgram env7
            "(if (< x y) 1 0)"

    env9 <-
        runProgram env8
            "(if (= x 10) 111 222)"

    _ <-
        runProgram env9
            "(if (= y 10) 111 222)"

    return ()