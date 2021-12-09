module Days.Day08
  ( runDay08
  ) where

import Results
import Utils
import System.IO.Unsafe
import Data.List
import Data.Maybe

-- | 'SignalPatterns' represent a list of signal patterns from the input
type SignalPatterns = [String]

-- | 'Output' represents the output numbers that require decoding
type Output = [String]

-- | a 'Display' is a pair of 'SignalPatterns' and 'Output' where the 'SignalPatterns' will be used
-- to decode the 'Output'
type Display = (SignalPatterns, Output)

-- | a 'DisplayConfig' is a '[String]' where each index represents the number of the display and the
-- value at each index represents the signal of the display number
type DisplayConfig = [String]

-- | 'runDay07' runs the 2 parts for Day 7 with the contents for a given file path
-- https://adventofcode.com/2021/day/7
runDay08 :: String -> Result
runDay08 filename = do
  let input = unsafePerformIO $ readFileLines filename
  let display = parseInput input
  let p1 = part1 display
  let p2 = part2 display
  (Just $ show p1, Just $ show p2)

{- Input parsing functions -}

-- | 'parseInput' parses each line of the input '[String]' into a 'Display'
parseInput :: [String] -> [Display]
parseInput = map parseInputLine 

-- | 'parseInputLine' parses one line of the input as a 'String' into a
-- 'Display'
parseInputLine :: String -> Display
parseInputLine str = (signalPatterns, digitOutputs)
  where
    (signalPatternsStr, digitOutputStr) = splitAtStr str " | "
    signalPatterns = splitStr signalPatternsStr ' '
    digitOutputs = splitStr digitOutputStr ' '

-- | 'part1' counts the amount of unique digits in the output
-- (i.e. 1's, 4's, 7's & 8's)
part1 :: [Display] -> Int
part1 displays = sum p1Counts
  where
    digitsFilter digits = length digits `elem` [2,4,3,7] -- Amounts for 1,4,7 & 8
    mapFunc = length . filter digitsFilter . snd
    p1Counts = map mapFunc displays

-- | 'part2' decodes, for each 'Display' in the given '[Display]', the 'SignalInput' into
-- a 'DisplayConfig' and uses that to calculate the pin for the 'Output' and sums the total of
-- all 'Output's calculated
part2 :: [Display] -> Int
part2 wiring = sum pins
  where
    pins = fromMaybe [-1] $ mapM decodePin wiring

-- | 'decodePin' decodes the pin for a 'Display' and returns it as a 'Maybe Int', where
-- 'Nothing' represents a malformed input
decodePin :: Display -> Maybe Int
decodePin (patterns, output) = do
  config <- createDisplayConf patterns
  digits <- mapM (decodeOutput config . sort) output
  return $ foldl (\acc d -> 10 * acc + d) 0 digits

-- | 'createDisplayConf' creates the display configuration based on a given 'SignalPatterns' and returns
-- the result as a 'Maybe DisplayConfig' where Nothing represents malformed input
createDisplayConf :: SignalPatterns -> Maybe DisplayConfig
createDisplayConf patterns = do
  (one, four, seven, eight) <- calcOneFourSevenEight patterns
  (zero, six, nine) <- calcZeroSixNine (filter ((==) 6 . length) patterns) (eight \\ (seven ++ four)) one
  (two, three, five) <- calcTwoThreeFive (filter ((==) 5 . length) patterns) one nine
  return $ map sort [zero, one, two, three, four, five, six, seven, eight, nine]

-- | 'decodeOutput' will decode a digit of the 'Output' into an 'MaybeInt' from a 'DisplayConfig', where
-- Nothing means the digit is not in the 'DisplayConfig'
decodeOutput :: DisplayConfig -> String -> Maybe Int
decodeOutput = flip elemIndex

-- | 'calcUniqueDisplay' calcuates the digit pattern for one, four, seven and eight, and returns them
-- in a tuple with that ordering, or Nothing if the input is malformed
calcOneFourSevenEight :: SignalPatterns -> Maybe (String, String, String, String)
calcOneFourSevenEight patterns = do
  one <- find ((==) 2 . length) patterns
  four <- find ((==) 4 . length) patterns
  seven <- find ((==) 3 . length) patterns
  eight <- find ((==) 7 . length) patterns
  return (one, four, seven, eight)

-- | 'calcZeroSixNine' calculates the digit pattern for zero, six, and nine from a pattern
-- of the bottom corner (e & g in the normal segment display on https://adventofcode.com/2021/day/8)
-- and from the one pattern and returns the patterns in a tuple of (zero, six, nine) or Nothing
-- if the input is invalid
calcZeroSixNine :: [String] -> String -> String -> Maybe (String, String, String)
calcZeroSixNine zeroSixNine  bottomTwo oneStr
  | length zeroSixNine /= 3 = Nothing
  | otherwise = Just (zeroStr, sixStr, nineStr)
        where
          isSix s = null (bottomTwo \\ s) && length (oneStr \\ s) == 1
          ([sixStr], [a, b]) = partition isSix zeroSixNine
          (zeroStr, nineStr) = if null (bottomTwo \\ a)
                               then (a, b)
                               else (b, a)

-- | 'calcTwoThreeFive' calculates the digit patter for two, three, and five from a
-- list of possible patterns, the pattern for one, and the pattern for nine, and returns a
-- tuple of the patterns (two, three, five) or Nothing if the input is malformed
calcTwoThreeFive :: [String] -> String -> String -> Maybe (String, String, String)
calcTwoThreeFive twoThreeFive one nine
  | length twoThreeFive /= 3 = Nothing
  | otherwise = Just (two, three, five)
    where
      isThree s = null (one \\ s)
      ([three], [a, b]) = partition isThree twoThreeFive
      (two, five) = if length (a \\ nine) == 1
                    then (a,b)
                    else (b,a)

