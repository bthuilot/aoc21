module Day where

import Days.Day01 
import Days.Day02

import Results

-- | 'Day' Represents a day for the Advent of Code
data Day = D01 -- ^ 'D01' represents Day 1
         | D02 -- ^ 'D02' represents Day 2
         | D03 -- ^ 'D03' represents Day3
         | Skip -- ^ 'Skip' represents a Day that should be skipped (when `--skip` CLI argument is used)

-- | Instance of 'Show' for 'Day' data type. Returns a formatted string of the name of the 'Day'
instance Show (Day) where
  show Skip = "Skipped"
  show D01 = "Day 1"
  show D02 = "Day 2"
  show _   = "Not implemented"


-- | 'runDay' will take in a 'Day' and a path to a file and return a result of running the Day's part 1 and part 2 problems
-- with the contents of the file as input
runDay :: Day -> String -> Result
runDay D01 f = runDay01 f
runDay D02 f = runDay02 f
runDay _ _ = (Nothing, Nothing)
