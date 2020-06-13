module IOComponents
    ( solveFromFile
    , printBoard
    ) where

import Solver


solveFromFile :: IO Board
solveFromFile = do
  file <- lines <$> readFile "problem.txt"
  let (strVerts, strHorizs') = span (/= "") file
      strHorizs = dropWhile (== "") strHorizs'
      ruleVerts  = map parseFileRow strVerts
      ruleHorizs = map parseFileRow strHorizs

  return $ solveFromRules (ruleVerts, ruleHorizs)
    where
      parseFileRow :: String -> [Int]
      parseFileRow = map read . words

printBoard :: Board -> IO ()
printBoard = putStr . showBoard

showBoard :: Board -> String
showBoard = unlines . map showRow

showRow :: Row -> String
showRow = map
  (\c ->
    case c of
      Nothing    -> '.'
      Just False -> '-'
      Just True  -> '#')
