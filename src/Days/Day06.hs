{-# LANGUAGE OverloadedStrings #-}

module Days.Day06
  ( runDay06
  ) where

import Results
import Utils
import System.IO.Unsafe
import qualified Data.Text as T
import qualified Data.Text.Read as R
import Data.Either
import Data.List

-- | 'FishCycles' represent the amount of fish in each stage of the fish life cycle,
-- where every index represents the day number and each value at an index represents the count
-- of fish for that day
type FishCycles = [Int]

-- | 'runDay06' runs the 2 parts for Day 6 with the contents for a given file path
-- https://adventofcode.com/2021/day/6
runDay06 :: String -> Result
runDay06 filename = do
  let input = unsafePerformIO $ readFileText filename
  let fish = parseInput input
  let day0 = createFishCycles fish
  let day80 = waitDays day0 79
  let day256 = waitDays day80 176
  let p1 = sum day80
  let p2 = sum day256
  (Just $ show p1, Just $ show p2)

{- Input parsing functions -}

-- | 'parseInput' parses 'T.Text' into a list of 'Line'
-- as per the spec https://adventofcode.com/2021/day/5
parseInput :: T.Text -> [Int]
parseInput t = map toDecimal (T.splitOn ","  t)
  where toDecimal = fst . fromRight (0, "")  . R.decimal

-- | 'createFishCycles' creates a 'FishCycles' from a list of 'Int' each value representing the day
-- in the fish life cycle a fish is on
createFishCycles :: [Int] -> FishCycles
createFishCycles fish = result ++ (take (9 - length result) $ repeat 0) 
  where result = map length (group sortedFish)
        sortedFish = sort fish

{- Part 1 & 2 logic functions -}

-- | 'waitDays' takes in a 'FishCycles' and computes the 'FishCycles' for the given
-- 'Int' days in the future
waitDays :: FishCycles -> Int -> FishCycles
waitDays fish i
  | i < 1 = fish
  | otherwise = waitDays nextDay (i - 1)
     where (rotated, prev0) = rotateL fish
           newBorns = rotated ++ [prev0]
           (before, (six:after)) = splitAt 6 newBorns
           nextDay = before ++ [(six + prev0)] ++ after
           
-- | 'rotateL' rotates a list of 'Int' to the left and returns the new list
-- in addition to the item removed, or 0 if the list is empty
rotateL :: [Int] -> ([Int], Int)
rotateL [] = ([], 0)
rotateL (a : xs) = (xs, a)
