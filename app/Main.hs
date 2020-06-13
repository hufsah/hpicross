module Main where

import IOComponents
import System.CPUTime
import Text.Printf

main :: IO ()
main = do
  startT <- getCPUTime

  solution <- solveFromFile
  case solution of
    Nothing -> putStrLn "Unsolvable"
    Just b  -> printBoard b

  endT <- getCPUTime

  let timeDiff = fromIntegral (endT - startT) / (10^9) :: Double
  printf "Finished in %.1f ms\n" timeDiff

