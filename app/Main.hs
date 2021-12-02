module Main where

import Day
import Results

inputDirectory :: String
inputDirectory = "assets/inputs/"

-- | 'days' is the collection of all Days along with their respecitve input file
days :: [(Day, String)]
days = [
  (D01, "day01.txt"),
  (D02, "day02.txt")
  ]

-- | 'outputFold' folds over all 'Result' and 'Day' data and concats all formatted output into one string
outputFold :: String -> (Day, Result) -> String
outputFold acc (Skip, _) = acc ++ "\nDay skipped"
outputFold acc (day, result) = acc ++ "\n" ++ (showResults dayName result)
  where
    dayName = show day

main :: IO ()
main = do
  let output = map (\(d, f) -> (d, (runDay d (inputDirectory ++ f)))) days
  let result = foldl outputFold "" output
  putStrLn $ result
