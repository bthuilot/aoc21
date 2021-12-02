module Day where

import Days.Day01 
import Days.Day02

import Results

data Day = D01
         | D02
         | D03
         | Skip

-- Instance of Show for Day data type
instance Show (Day) where
  show Skip = "Skipped"
  show D01 = "Day 1"
  show D02 = "Day 2"
  show _   = "Not implemented"


-- | 'runDay' will take in a 'Day' and a path to a file and return a result of running the Day's part 1 and part 2 problems
-- | with the contents of the file as input
runDay :: Day -> String -> Result
runDay D01 f = runDay01 f
runDay D02 f = runDay02 f
runDay _ _ = (Nothing, Nothing)
