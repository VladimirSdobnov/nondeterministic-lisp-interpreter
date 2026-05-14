module Main where

import Parser

main :: IO ()
main = print (tokenize "(+ (+ 1 2) (+ 32 4))")