module Main where

import Control.Exception
import Parser

runTest :: String -> IO ()
runTest input = do

    putStrLn "================================="
    putStrLn ("Input: " ++ input)

    result <- try (evaluateParser input)
        :: IO (Either SomeException ())

    case result of

        Left ex -> do
            putStrLn "Error:"
            print ex

        Right _ ->
            putStrLn "Success"

-- Выполняет tokenize + parse
evaluateParser :: String -> IO ()
evaluateParser input = do

    let tokens = tokenize input

    putStrLn "Tokens:"
    print tokens

    putStrLn "AST:"
    print (parse tokens)

main :: IO ()
main = do

    runTest "(+ 1 (* 2 3))"

    runTest "(+ 1 (* 2 3))))"

    runTest "(+ 1 (* 2 3)"

    runTest ")"

    runTest "#x"