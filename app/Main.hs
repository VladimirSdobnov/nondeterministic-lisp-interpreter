module Main where

import Evaluator
import Parser
import Primitives
import Environment

runProgram :: Env -> String -> IO Env
runProgram env input = do

    let tokens =
            tokenize input

    let expr =
            parse tokens

    let (result, newEnv) =
            eval env expr

    print result

    return newEnv

main :: IO ()
main = do

    env1 <-
        runProgram primitiveEnv
            "(define x 10)"

    _ <-
        runProgram env1
            "(+ x 5)"

    return ()