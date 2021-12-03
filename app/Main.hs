module Main where

import Day
import Results
import System.Exit
import System.Environment

-- | 'DayOptions' represent the options for which 'Day' parts to run
data DayOptions = OnlyDays [Int] -- | ^ 'OnlyDays' is a list of day numbers to only run
                | SkipDays [Int] -- | ^ 'SkipDays' is a list of day numbers to skip when running all days
                | AllDays -- | ^ 'AllDays' represents running all tests

-- | 'inputDirectory' is the path to the input directory, relative to the root of the repository
inputDirectory :: String
inputDirectory = "./assets/inputs/"

-- | 'days' is the collection of all Days along with their respecitve input file
days :: [(Day, String)]
days = [
  (D01, "day01.txt"),
  (D02, "day02.txt"),
  (D03, "day03.txt")
  ]

-- | 'outputFold' folds over all 'Result' and 'Day' data and concats all formatted output into one string
outputFold :: String -> (Day, Result) -> String
outputFold acc (Skip, _) = acc ++ "\nDay skipped"
outputFold acc (day, result) = acc ++ "\n" ++ (showResults dayName result)
  where
    dayName = show day

-- | 'parseArgs' parses the arguments supplied to the script into a 'DayOptions'
parseArgs :: [String] -> IO DayOptions
parseArgs [] = return $ AllDays
parseArgs (flag : xs)
  | flag == "-o" || flag == "--only" = return $ OnlyDays (map read xs)
  | flag == "-s" || flag == "--skip" = return $ SkipDays (map read xs)
  | otherwise = do
      putStrLn $ "Invalid argument " ++ flag
      exitWith (ExitFailure 1)
      
-- | 'selectDays' filters a list of '(Day, String)' to be run based on its index (day number)
-- and the parsed DayOptions
selectDays :: Int -> DayOptions -> [(Day, String)] -> [(Day, String)]
selectDays _ _ [] = []
selectDays _ AllDays dayInputs = dayInputs
selectDays i opts@(OnlyDays only) (d : xs)
  | i `elem` only = d : recur
  | otherwise = recur
    where recur = selectDays (i + 1) opts xs
selectDays i opts@(SkipDays skip) (d : xs)
  | i `elem` skip = recur
  | otherwise = d : recur
    where recur = selectDays (i + 1) opts xs

-- | 'main' entry point of Advent of Code 2021
main :: IO ()
main = do
  dayOptions <- getArgs >>= parseArgs
  let daysToRun = selectDays 1 dayOptions days 
  let output = map (\(d, f) -> (d, (runDay d (inputDirectory ++ f)))) daysToRun
  let result = foldl outputFold "" output
  putStrLn $ result
