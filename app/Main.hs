module Main where

import Environment
import Value

main :: IO ()
main = do

    let env1 =
            defineVar "x" (NumberV 42) emptyEnv

    print (lookupVar "x" env1)

    print (lookupVar "y" env1)