module Main where

import AmbEvaluator
import Parser
import Primitives

import Data.IORef
import System.IO.Unsafe
import Prelude hiding (fail)

-- ============================================================
-- try-again infrastructure
-- ============================================================

nextResult :: IORef (Maybe FailureCont)

{-# NOINLINE nextResult #-}
nextResult =
    unsafePerformIO (newIORef Nothing)

printSuccess :: SuccessCont
printSuccess value _ fail = do

    putStrLn ("Result: " ++ show value)

    writeIORef nextResult (Just fail)

printFailure :: FailureCont
printFailure = do

    putStrLn "No more results"

    writeIORef nextResult Nothing

runProgram :: String -> IO ()
runProgram input = do

    putStrLn "================================="
    putStrLn "Program:"
    putStrLn input
    putStrLn ""

    let tokens =
            tokenize input

    let expr =
            parse tokens

    eval
        primitiveEnv
        expr
        printSuccess
        printFailure

tryAgain :: IO ()
tryAgain = do

    next <- readIORef nextResult

    case next of

        Just fail ->
            fail

        Nothing ->
            putStrLn "No saved continuation"

-- ============================================================
-- Multiple Dwelling Puzzle
-- ============================================================

multipleDwelling :: String

multipleDwelling =
    concat
        [
            "(begin ",

            -- =================================================
            -- Choices
            -- =================================================

            "(define baker (amb 1 2 3 4 5)) ",
            "(define cooper (amb 1 2 3 4 5)) ",
            "(define fletcher (amb 1 2 3 4 5)) ",
            "(define miller (amb 1 2 3 4 5)) ",
            "(define smith (amb 1 2 3 4 5)) ",

            -- =================================================
            -- Distinct floors
            -- =================================================

            "(require (not (= baker cooper))) ",
            "(require (not (= baker fletcher))) ",
            "(require (not (= baker miller))) ",
            "(require (not (= baker smith))) ",

            "(require (not (= cooper fletcher))) ",
            "(require (not (= cooper miller))) ",
            "(require (not (= cooper smith))) ",

            "(require (not (= fletcher miller))) ",
            "(require (not (= fletcher smith))) ",

            "(require (not (= miller smith))) ",

            -- =================================================
            -- Constraints
            -- =================================================

            -- Baker does not live on top floor
            "(require (not (= baker 5))) ",

            -- Cooper does not live on bottom floor
            "(require (not (= cooper 1))) ",

            -- Fletcher not on top or bottom
            "(require (not (= fletcher 5))) ",
            "(require (not (= fletcher 1))) ",

            -- Miller lives above Cooper
            "(require (> miller cooper)) ",

            -- Smith not adjacent to Fletcher
            "(require (not (= (abs (- smith fletcher)) 1))) ",

            -- Fletcher not adjacent to Cooper
            "(require (not (= (abs (- fletcher cooper)) 1))) ",

            -- =================================================
            -- Result
            -- =================================================

            "(list ",

                "(list (quote baker) baker) ",
                "(list (quote cooper) cooper) ",
                "(list (quote fletcher) fletcher) ",
                "(list (quote miller) miller) ",
                "(list (quote smith) smith)",

            "))"
        ]

-- ============================================================
-- Main
-- ============================================================

main :: IO ()
main = do

    runProgram multipleDwelling