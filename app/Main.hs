module Main where

import IOComponents

main :: IO ()
main = solveFromFile >>= printBoard
