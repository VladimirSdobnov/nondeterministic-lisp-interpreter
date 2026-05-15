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

    putStrLn "========== BASIC LIST TESTS =========="

    env1 <-
        runProgram primitiveEnv
            "(list 1 2 3 4 5)"

    env2 <-
        runProgram env1
            "(car (list 10 20 30))"

    env3 <-
        runProgram env2
            "(cdr (list 10 20 30))"

    env4 <-
        runProgram env3
            "(cons 0 (list 1 2 3))"

    env5 <-
        runProgram env4
            "(null? (list))"

    env6 <-
        runProgram env5
            "(null? (list 1 2 3))"

    putStrLn "========== LIST + RECURSION TEST =========="

    env7 <-
        runProgram env6
            (concat
                [
                    "(begin ",

                    "(define length ",
                        "(lambda (lst) ",
                            "(if (null? lst) ",
                                "0 ",
                                "(+ 1 (length (cdr lst)))))) ",

                    "(length (list 1 2 3 4 5 6 7)))"
                ])

    putStrLn "========== LIST PROCESSING TEST =========="

    env8 <-
        runProgram env7
            (concat
                [
                    "(begin ",

                    "(define sum-list ",
                        "(lambda (lst) ",
                            "(if (null? lst) ",
                                "0 ",
                                "(+ (car lst) ",
                                   "(sum-list (cdr lst)))))) ",

                    "(sum-list (list 1 2 3 4 5)))"
                ])

    putStrLn "========== SYMBOLIC LIST TEST =========="

    _ <-
        runProgram env8
            "(cons (list 1 2) (list 3 4 5))"

    return ()