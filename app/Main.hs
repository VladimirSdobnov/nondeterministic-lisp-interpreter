module Main where

import Primitives
import Value

main :: IO ()
main = do

    putStrLn "================================="
    putStrLn "Arithmetic primitives"
    putStrLn "================================="

    -- (+ 1 2 3) -> 6
    print
        (primitivePlus
            [NumberV 1, NumberV 2, NumberV 3])

    -- (+) -> 0
    print
        (primitivePlus [])

    -- (- 10 3 2) -> 5
    print
        (primitiveMinus
            [NumberV 10, NumberV 3, NumberV 2])

    -- (- 5) -> -5
    print
        (primitiveMinus
            [NumberV 5])

    -- (* 2 3 4) -> 24
    print
        (primitiveMultiply
            [NumberV 2, NumberV 3, NumberV 4])

    -- (*) -> 1
    print
        (primitiveMultiply [])

    -- (/ 20 2 2) -> 5
    print
        (primitiveDivide
            [NumberV 20, NumberV 2, NumberV 2])

    -- (/ 5 2) -> 2
    print
        (primitiveDivide
            [NumberV 5, NumberV 2])

    putStrLn ""
    putStrLn "================================="
    putStrLn "Comparison primitives"
    putStrLn "================================="

    -- (= 5 5) -> #t
    print
        (primitiveEq
            [NumberV 5, NumberV 5])

    -- (= 5 3) -> #f
    print
        (primitiveEq
            [NumberV 5, NumberV 3])

    -- (< 2 5) -> #t
    print
        (primitiveLess
            [NumberV 2, NumberV 5])

    -- (< 7 3) -> #f
    print
        (primitiveLess
            [NumberV 7, NumberV 3])

    -- (> 10 3) -> #t
    print
        (primitiveGreater
            [NumberV 10, NumberV 3])

    -- (> 1 8) -> #f
    print
        (primitiveGreater
            [NumberV 1, NumberV 8])