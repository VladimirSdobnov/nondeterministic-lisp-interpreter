module Main where

import AmbEvaluator
import Parser
import Primitives
import Value
import Prelude hiding (fail)

printSuccess :: SuccessCont
printSuccess value _ fail = do
    putStrLn ("Result: " ++ show value)
    fail

printFailure :: FailureCont
printFailure =
    putStrLn "No more results"

runProgram :: String -> IO ()
runProgram input = do
    putStrLn "================================="
    putStrLn ("Program: " ++ input)

    let tokens =
            tokenize input

    let expr =
            parse tokens

    eval primitiveEnv expr printSuccess printFailure

main :: IO ()
main = do

    runProgram "(amb 1 2 3)"

    runProgram "(+ (amb 1 2 3) 10)"

    runProgram "(begin (define x (amb 1 2 3 4 5)) (define y (amb 10 20 30)) (require (> (+ x y) 22)) (list x y))"