module Day where

import Days.Day01 
import Days.Day02
import Days.Day03
import Days.Day04
import Days.Day05
import Days.Day06
import Days.Day07
import Days.Day08
import Days.Day10

import Results

-- | 'Day' Represents a day for the Advent of Code
data Day = D01
         | D02
         | D03
         | D04
         | D05
         | D06
         | D07
         | D08
         | D09
         | D10
         | D11
         | D12
         | D13
         | D14
         | D15
         | D16
         | D17
         | D18
         | D19
         | D20
         | D21
         | D22
         | D23
         | D24
         | D25
         | Skip -- ^ 'Skip' represents a Day that should be skipped (when `--skip` CLI argument is used)

-- | Instance of 'Show' for 'Day' data type. Returns a formatted string of the name of the 'Day'
instance Show Day where
  show Skip = "Skipped"
  show D01 = "Day 1"
  show D02 = "Day 2"
  show D03 = "Day 3"
  show D04 = "Day 4"
  show D05 = "Day 5"
  show D06 = "Day 6"
  show D07 = "Day 7"
  show D08 = "Day 8"
  show D10 = "Day 10"
  show _   = "Not implemented"


-- | 'runDay' will take in a 'Day' and a path to a file and return a result of running the Day's part 1 and part 2 problems
-- with the contents of the file as input
runDay :: Day -> String -> Result
runDay D01 f = runDay01 f
runDay D02 f = runDay02 f
runDay D03 f = runDay03 f
runDay D04 f = runDay04 f
runDay D05 f = runDay05 f
runDay D06 f = runDay06 f
runDay D07 f = runDay07 f
runDay D08 f = runDay08 f
runDay D10 f = runDay10 f
runDay _ _ = (Nothing, Nothing)
