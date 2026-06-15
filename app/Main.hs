module Main where

import qualified Evaluator as Det
import qualified AmbEvaluator as Amb

import qualified DeterministicPrograms as DetPrograms
import qualified NondeterministicPrograms as AmbPrograms

import Data.IORef
import Data.Time.Clock
import Parser
import Primitives
import Value

-- ============================================================
-- Settings
-- ============================================================

numericIterations :: Int
numericIterations =
    10

logicalIterations :: Int
logicalIterations =
    1000

numericSizes :: [Integer]
numericSizes =
    [20, 40, 80, 160]

-- ============================================================
-- Forcing evaluation
-- ============================================================

forceValue :: Value -> IO ()
forceValue value =
    valueSize value `seq` return ()

valueSize :: Value -> Int
valueSize (NumberV n) =
    n `seq` 1

valueSize (BooleanV b) =
    b `seq` 1

valueSize (SymbolV s) =
    length s `seq` 1

valueSize (ListV values) =
    foldr
        (\value acc -> valueSize value + acc)
        1
        values

valueSize (PrimitiveFunc _) =
    1

valueSize (Closure {}) =
    1

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
    -- putStrLn ("Result size: " ++ show (valueSize firstResult))
    -- putStrLn ("Runs: " ++ show count)
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
-- Numeric benchmark
-- ============================================================

-- runNumericBenchmark :: Integer -> IO ()
-- runNumericBenchmark n = do
--     putStrLn ""
--     putStrLn "============================================================"
--     putStrLn ("Numeric task, n = " ++ show n)
--     putStrLn "============================================================"

--     measureAverage
--         ("Deterministic program on deterministic evaluator: first solution, n = " ++ show n)
--         numericIterations
--         (runDeterministic (DetPrograms.threeNumbersFirst n))

--     measureAverage
--         ("Deterministic program on deterministic evaluator: all solutions, n = " ++ show n)
--         numericIterations
--         (runDeterministic (DetPrograms.threeNumbersAll n))

--     measureAverage
--         ("Deterministic program on nondeterministic evaluator: first solution, n = " ++ show n)
--         numericIterations
--         (runNondeterministic (DetPrograms.threeNumbersFirst n))

--     measureAverage
--         ("Deterministic program on nondeterministic evaluator: all solutions, n = " ++ show n)
--         numericIterations
--         (runNondeterministic (DetPrograms.threeNumbersAll n))

--     measureAverage
--         ("Nondeterministic program on nondeterministic evaluator: first solution, n = " ++ show n)
--         numericIterations
--         (runNondeterministic (AmbPrograms.threeNumbersFirst n))

--     measureAverage
--         ("Nondeterministic program on nondeterministic evaluator: all solutions, n = " ++ show n)
--         numericIterations
--         (runNondeterministic (AmbPrograms.threeNumbersAll n))

-- ============================================================
-- Logical benchmark
-- ============================================================

runLogicalBenchmark :: IO ()
runLogicalBenchmark = do
    putStrLn ""
    putStrLn "============================================================"
    putStrLn "Logical task: SICP Multiple Dwelling"
    putStrLn "============================================================"

    measureAverage
        "Deterministic program on deterministic evaluator: first solution"
        logicalIterations
        (runDeterministic DetPrograms.multipleDwellingFirst)

    measureAverage
        "Deterministic program on deterministic evaluator: all solutions"
        logicalIterations
        (runDeterministic DetPrograms.multipleDwellingAll)

    measureAverage
        "Deterministic program on nondeterministic evaluator: first solution"
        logicalIterations
        (runNondeterministic DetPrograms.multipleDwellingFirst)

    measureAverage
        "Deterministic program on nondeterministic evaluator: all solutions"
        logicalIterations
        (runNondeterministic DetPrograms.multipleDwellingAll)

    measureAverage
        "Nondeterministic program on nondeterministic evaluator: first solution"
        logicalIterations
        (runNondeterministic AmbPrograms.multipleDwellingFirst)

    measureAverage
        "Nondeterministic program on nondeterministic evaluator: all solutions"
        logicalIterations
        (runNondeterministic AmbPrograms.multipleDwellingAll)

-- ============================================================
-- Main
-- ============================================================

main :: IO ()
main = do
    --mapM_ runNumericBenchmark numericSizes
    runLogicalBenchmark