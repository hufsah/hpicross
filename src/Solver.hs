module Solver
    ( Row
    , Board
    , solveFromRules
    ) where

import Data.List (transpose)


type Cell = Maybe Bool

type Row = [Cell]

type BoardSize = (Int, Int)

type Board = [Row]

type Rule = [Int]

type Rules1D = [Rule]

type Rules = (Rules1D, Rules1D)

type Game = (Board, Rules)

getBoard :: Game -> Board
getBoard = fst

solveFromRules :: Rules -> Maybe Board
solveFromRules (rvs, rhs) =
  getBoard <$> solve (emptyBoard, (rvs, rhs))
    where emptyBoard = replicate (length rvs) $ replicate (length rhs) Nothing

solve :: Game -> Maybe Game
solve g =
  if isFilled (getBoard g)
  then Just g
  else let g' = lockSome g in
    if getBoard g == getBoard g'
    then Nothing
    else solve g'

lockSome :: Game -> Game
lockSome (b, (rvs, rhs)) =
  let b' =  lock1D b rvs
      b'' = lock1D (transpose b') rhs
  in (transpose b'', (rvs, rhs))

lock1D :: Board -> Rules1D -> Board
lock1D = zipWith lockRow

lockRow :: Row -> Rule -> Row
lockRow row rule =
  if length candidates == 0
  then row
  else intersection candidates
    where
      candidates = filter (not . contradicts row) $ allRows (length row) rule

intersection :: [Row] -> Row
intersection = foldl1 (zipWith (\x y -> if x == y then x else Nothing)) 

contradicts :: Row -> Row -> Bool
contradicts []     []     = False
contradicts (x:xs) (y:ys) =
  if x == y
  then contradicts xs ys
  else case (x, y) of
    (Just True,  Just False) -> True
    (Just False, Just True)  -> True
    _                        -> contradicts xs ys

allRows :: Int -> Rule -> [Row]
allRows n      []     = [replicate n $ Just False]
allRows rowLen (n:ns) =
  if n + sum ns + length ns == rowLen
  then [fillCompactRow (n:ns)]
  else    [head1 ++ t | t <- allRows (rowLen - n - 1) ns]
       ++ [head2 ++ t | t <- allRows (rowLen - 1) (n:ns)]
    where
      head1 = replicate n (Just True) ++ [Just False]
      head2 = [Just False]

fillCompactRow :: Rule -> Row
fillCompactRow = tail . fillCompactRow'
  where
    fillCompactRow' []     = []
    fillCompactRow' (n:ns) =
      Just False : replicate n (Just True) ++ fillCompactRow' ns

isFilled :: Board -> Bool
isFilled b = and $ map (/= Nothing) $ concat b
