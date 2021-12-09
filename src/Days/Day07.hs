{-# LANGUAGE OverloadedStrings #-}

module Days.Day07
  ( runDay07
  ) where

import Results
import Utils
import System.IO.Unsafe
import qualified Data.Text as T
import qualified Data.Text.Read as R
import Data.Either
import Data.List

type CrabPositions = [Int]

-- | 'runDay07' runs the 2 parts for Day 7 with the contents for a given file path
-- https://adventofcode.com/2021/day/7
runDay07 :: String -> Result
runDay07 filename = do
  let input = unsafePerformIO $ readFileText filename
  let crabs = sort $ parseInput input
  let p1 = part1 crabs
  let p2 = part2 crabs
  (Just $ show p1, Just $ show p2)

{- Input parsing functions -}

-- | 'parseInput' parses 'T.Text' into a list of 'Int'
-- as per the spec https://adventofcode.com/2021/day/7
parseInput :: T.Text -> CrabPositions
parseInput t = map toDecimal (T.splitOn ","  t)
  where toDecimal = fst . fromRight (0, "")  . R.decimal

-- | 'part1' calculates minimum the total fuel cost of each crab in 'CrabPosition'
-- where each move only cost 1 fuel
part1 :: CrabPositions -> Int
part1 crabs = fuelCost crabs median
  where median = findMedian crabs

-- | 'part2' calculates minimum the total fuel cost of each crab in 'CrabPosition'
-- where each consecutive move costs 1 more fuel
part2 :: CrabPositions -> Int
part2 crabs = min (fuelCost2 crabs meanFloor) (fuelCost2 crabs meanCeil)
  where meanFloor = floor $ findMean crabs -- TODO: Fix this
        meanCeil = ceiling $ findMean crabs

-- | 'fuelCost' calculates the total movement cost for each crab in 'CrabPositions' to move
-- to the spot at 'Int' given each move costs 1 fuel
fuelCost :: CrabPositions -> Int -> Int
fuelCost crabs i =  foldl (\acc crab -> abs (crab - i) + acc) 0 crabs

-- | 'fuelCost2' calculates the total movement cost for each crab in 'CrabPositions' to move
-- to the spot at 'Int' given each consecutive move costs 1 additional fuel
fuelCost2 :: [Int] -> Int -> Int
fuelCost2 crabs i = foldl (\acc crab -> costFunc (abs (crab - i)) + acc) 0 crabs
  where costFunc n = (n * (n + 1)) `div` 2

-- | 'findMedian' finds the median of a list of 'Int'
findMedian :: [Int] -> Int
findMedian [] = 0
findMedian xs = xs !! mid
  where mid = length xs `div` 2

-- | 'findMean' finds the mean of a given list of 'Int'
findMean :: [Int] -> Double
findMean [] = 0
findMean xs = total / len
  where total = fromIntegral (sum xs)
        len = fromIntegral (length xs)
