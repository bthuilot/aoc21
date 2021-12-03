

module Days.Day01 (
  runDay01
  ) where

import Results
import Utils
import System.IO.Unsafe

-- | 'runDay01' will run Day 1 of AdventOfCode with the contents from the given file name
runDay01 :: String -> Result
runDay01 filename = do
  let ints = unsafePerformIO $ readFileLinesInts filename
  let part1Result = part1 ints
  let part2Result = part2 ints
  (Just $ show part1Result, Just $ show part2Result)

-- | 'part1' computes the number of times a depth (an integer in the input list) increases from the previous.
-- https://adventofcode.com/2021/day/1#part1
part1 :: [Int] -> Int
part1 (a : b : xs)
  | a < b = 1 + recur
  | otherwise = recur
  where
    recur = part1 (b : xs)
part1 _ = 0


-- | 'part2' computes the number of times a 3 sliding window depth (a grouping of 3 consecutive integers in the input list). increases from the previous sliding window
-- https://adventofcode.com/2021/day/1#part2
part2 :: [Int] -> Int
part2 depths@(a : b : c : d : _)
  | window1 < window2 = 1 + recur
  | otherwise = recur
  where
    window1 = a + b + c
    window2 = b + c + d
    recur = part2 (tail depths)
part2 _ = 0
