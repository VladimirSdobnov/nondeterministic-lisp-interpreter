module Main where

import AST

main :: IO ()
main = print (List [Symbol "+", Number 1, Number 2])