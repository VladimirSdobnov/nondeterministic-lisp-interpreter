module Main where

import qualified Evaluator as Det
import qualified AmbEvaluator as Amb

import qualified DeterministicPrograms as DetPrograms
import qualified NondeterministicPrograms as AmbPrograms

import AST
import Data.IORef
import Data.Time.Clock
import Parser
import Primitives
import Value

-- ============================================================
-- Settings
-- ============================================================

iterations :: Int
iterations =
    1000

-- ============================================================
-- Forcing evaluation
-- ============================================================

-- Принудительно вычисляет результат интерпретатора.
--
-- Это важно для детерминированной версии:
-- из-за ленивости Haskell результат может оставаться thunk-ом,
-- если его явно не использовать.
forceValue :: Value -> IO ()
forceValue value =
    length (show value) `seq` return ()

-- ============================================================
-- Benchmark helpers
-- ============================================================

measureAverage :: String -> Int -> IO Value -> IO ()
measureAverage title count action = do
    putStrLn ""
    putStrLn ("========== " ++ title ++ " ==========")

    start <-
        getCurrentTime

    firstResult <-
        action

    forceValue firstResult

    runRepeatedly (count - 1) action

    end <-
        getCurrentTime

    let
        totalTime =
            diffUTCTime end start

        avgSeconds =
            realToFrac totalTime / fromIntegral count :: Double

    putStrLn ("Result: " ++ show firstResult)
    putStrLn ("Runs: " ++ show count)
    putStrLn ("Total time: " ++ show totalTime)
    putStrLn ("Average time: " ++ show avgSeconds ++ " sec")

runRepeatedly :: Int -> IO Value -> IO ()
runRepeatedly count action =
    if count <= 0
        then
            return ()
        else do
            result <-
                action

            forceValue result

            runRepeatedly (count - 1) action

-- ============================================================
-- Deterministic runner
-- ============================================================

-- Важно:
-- parse/tokenize находятся внутри функции,
-- поэтому время парсинга входит в общий замер.
runDeterministic :: String -> IO Value
runDeterministic input = do

    let
        expr =
            parse (tokenize input)

        (result, _) =
            Det.eval primitiveEnv expr

    return result

-- ============================================================
-- Nondeterministic runner
-- ============================================================

-- Важно:
-- parse/tokenize также находятся внутри функции,
-- поэтому время парсинга входит в общий замер.
runNondeterministic :: String -> IO Value
runNondeterministic input = do

    let
        expr =
            parse (tokenize input)

    resultRef <-
        newIORef Nothing

    Amb.eval
        primitiveEnv
        expr
        (saveFirstResult resultRef)
        noSolution

    result <-
        readIORef resultRef

    case result of

        Just value ->
            return value

        Nothing ->
            return (SymbolV "no-solution")

saveFirstResult :: IORef (Maybe Value) -> Amb.SuccessCont
saveFirstResult resultRef value _ _ =
    writeIORef resultRef (Just value)

noSolution :: Amb.FailureCont
noSolution =
    return ()

-- ============================================================
-- Main
-- ============================================================

main :: IO ()
main = do

    measureAverage
        "Deterministic: three numbers, first solution"
        iterations
        (runDeterministic DetPrograms.threeNumbersFirst)

    measureAverage
        "Deterministic: three numbers, all solutions"
        iterations
        (runDeterministic DetPrograms.threeNumbersAll)

    measureAverage
        "Nondeterministic: three numbers, first solution"
        iterations
        (runNondeterministic AmbPrograms.threeNumbersFirst)

    measureAverage
        "Nondeterministic: three numbers, all solutions"
        iterations
        (runNondeterministic AmbPrograms.threeNumbersAll)

    measureAverage
        "Deterministic: multiple dwelling, first solution"
        iterations
        (runDeterministic DetPrograms.multipleDwellingFirst)

    measureAverage
        "Deterministic: multiple dwelling, all solutions"
        iterations
        (runDeterministic DetPrograms.multipleDwellingAll)

    measureAverage
        "Nondeterministic: multiple dwelling, first solution"
        iterations
        (runNondeterministic AmbPrograms.multipleDwellingFirst)

    measureAverage
        "Nondeterministic: multiple dwelling, all solutions"
        iterations
        (runNondeterministic AmbPrograms.multipleDwellingAll)