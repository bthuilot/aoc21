module Day where

import Days.Day01 


data Day = D01
         | D02
         | D03

runDay :: Day -> String -> String
runDay D01 f = runDay01 f
runDay _ _ = "Skip"
