module Main where

import AmbEvaluator
import Parser
import Primitives

printCont :: Continuation
printCont value _ =
    putStrLn ("Result: " ++ show value)

runProgram :: String -> IO ()
runProgram input = do

    let tokens =
            tokenize input

    let expr =
            parse tokens

    eval primitiveEnv expr printCont

main :: IO ()
main = do

    putStrLn "========== CPS APPLICATION TESTS =========="

    runProgram "(+ 1 2)"

    runProgram "(* 2 3 4)"

    runProgram "(+ (* 2 3) 10)"