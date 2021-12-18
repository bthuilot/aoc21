{-# LANGUAGE OverloadedStrings #-}

module Days.Day09
  ( runDay09
  ) where

import Results
import Utils
import System.IO.Unsafe
import qualified Data.Text as T
import qualified Data.Text.Read as R
import Data.Either
import Data.List
import Data.Char(digitToInt)


-- | 'runDay09' runs the 2 parts for Day 9 with the contents for a given file path
-- https://adventofcode.com/2021/day/7
runDay09 :: String -> Result
runDay09 filename = do
  let input = unsafePerformIO $ readFileLines filename
  let nums = parseInput input
  let p1 = findLows input
  (Nothing, Nothing)

{- Input parsing functions -}

parseInput :: [String] -> [[Int]]
parseInput input = map parseIntList input
  where
    parseIntList str = map digitToInt str


part1 :: [[Int]] -> [Int]
part1 input
  | length input < 3 = []
  | othwerwise = above ++ lows ++ below
  where
    lows = fLow input
    (t:r:xs) = input
    above = findLows r t Nothing
    last = (last input)
    below = findLows (last . init $ input) Nothing last
  

fLow :: [[Int]] -> [Int]
fLow (a:r:b:xs) = lows ++ recur
  where
    lows = findLows r a b
    recur = fLow (r:b:xs)
fLow _ = []
    

findLows :: [Int] -> Maybe [Int] -> Maybe [Int] -> [Int]
findLows row Nothing below = zip row below
findLows row above Nothing = zip row above
findLows row above below = findLow4 zipped
  where zipped = zip3 above row below

findLow4 :: [(Int, Int, Int)] -> [Int]
findLow4 ((_, w, _):x@(n, i, s):y@(_, e, _):xs)
  | all (> i) [n,s,e,w] = (i : recur)
  | otherwise = recur
  where
    recur = findLow3 (x : y : xs)
findLow4 _ = []

findLow3 :: (Int, Int, Int) -> (Int, Int, Int) -> [Int]
findLow3 (n,i,s) (_, l, _) = if (all (> i) [n,s,l]) then i else 0

findLow2 :: [(Int, Int)] -> [Int]
findLow2 

  
