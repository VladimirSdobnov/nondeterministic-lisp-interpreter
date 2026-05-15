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

    return newEnv

main :: IO ()
main = do

    _ <-
        runProgram primitiveEnv
            (concat
                [
                    "(begin ",

                    "(define fact ",
                        "(lambda (n) ",
                            "(if (= n 0) ",
                                "1 ",
                                "(* n (fact (- n 1)))))) ",

                    "(define fib ",
                        "(lambda (n) ",
                            "(if (< n 2) ",
                                "n ",
                                "(+ (fib (- n 1)) ",
                                   "(fib (- n 2)))))) ",

                    "(define power ",
                        "(lambda (x n) ",
                            "(if (= n 0) ",
                                "1 ",
                                "(* x (power x (- n 1)))))) ",

                    "(+ (fact 6) ",
                       "(fib 12) ",
                       "(power 2 10)))"
                ])

    return ()