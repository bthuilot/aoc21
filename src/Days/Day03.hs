module Days.Day03
  ( runDay03
  ) where

import Results
import Utils
import System.IO.Unsafe

-- | 'BinaryDigit' represents a digit in binary, either 1 or 0
data BinaryDigit = Zero -- ^ 'Zero' represents the value 0
                 | One -- ^ 'One' represents the value 1
                 | Invalid -- ^ 'Invalid' represents an input where the binary number contains a digit other than 1 or 0 

-- | 'Eq' Instance to compare 'BinaryDigit'
instance Eq BinaryDigit where
  One == One = True
  Zero == Zero = True
  _ == _ = False

instance Show BinaryDigit where
  show One = "1"
  show Zero = "0"
  show Invalid = "?"

-- | 'BinaryNum' is list of 'BinaryDigit' that represent a binary number
-- the most significant bit is located at index 0
type BinaryNum = [BinaryDigit]

{- Conversion functions for BinaryDigit's and BinaryNum's-}

-- | 'charToBinDigit' converts a character representation of a digit in binary to a 'BinaryDigit'.
-- 0 becomes 'Zero', one becomes 'One' and any other character becomes 'Invalid'
charToBinDigit :: Char -> BinaryDigit
charToBinDigit c
  | c == '1' = One
  | c == '0' = Zero
  | otherwise = Invalid

-- | 'binDigitToInto' converts a 'BinaryDigit' into its integer representation.
-- 'Invalid' becomes -1
binDigitToInt :: BinaryDigit -> Int
binDigitToInt d
  | d == Zero = 0
  | d == One = 1
  | otherwise = -1

-- | 'binNumToInt' converts a 'BinaryNumber' to an 'Int' representation
binNumToInt :: BinaryNum -> Int
binNumToInt [] = 0
binNumToInt (d : xs) = val * (2 ^ length xs) + recur
  where val = binDigitToInt d
        recur = binNumToInt xs

-- | 'runDay03' runs the 2 parts for Day 3 with the contents for a given file path
-- https://adventofcode.com/2021/day/3
runDay03 :: String -> Result
runDay03 filename = do
  let inputLines = unsafePerformIO $ readFileLines filename
  let binNum = parseInput inputLines
  let part1Result = part1 binNum
  let part2Result = part2 binNum
  (Just $ show part1Result, Just $ show part2Result)

-- | 'parseInput' parses a list of strings into a list of 'BinaryNumber'
-- each line should consist of the same length of 1's and 0's
parseInput :: [String] -> [BinaryNum]
parseInput [] = []
parseInput (num : xs) = binNum : parseInput xs
  where binNum = map charToBinDigit num

-- | 'part1' takes in a list of 'BinaryNumber's and finds the gamma and eplison rates as per
-- https://adventofcode.com/2021/day/3#part1 and returns their product
part1 :: [BinaryNum] -> Int
part1 [] = 0
part1 nums = binNumToInt revMcbs * binNumToInt revLcbs
  where len = length (head nums) - 1
        (mcbs, lcbs) = getCommonBits nums len -- get most and least common bits
        revMcbs = reverse mcbs
        revLcbs = reverse lcbs
  
-- | 'bitsAt' takes in a list of 'BinaryNum' and a position and
-- returns a list of bits at each of the positions
bitsAt :: [BinaryNum] -> Int -> [BinaryDigit]
bitsAt num pos = map getBitAt num
  where getBitAt = flip (!!) pos

-- | 'getCommonBitsAt' takes in a list of 'BinaryNum's and an 'Int' representing an index and returns a pair of 'BinaryDigit'
-- the first representing the most common bit and the second representing the least common bit
getCommonBitsAt :: [BinaryNum] -> Int -> (BinaryDigit, BinaryDigit)
getCommonBitsAt nums i = if zeroCount > oneCount then (Zero, One) else (One, Zero)
  where bits = bitsAt nums i
        zeroCount = length $ filter (==Zero) bits
        oneCount = length $ filter (==One) bits

-- | 'getCommonBits' takes in a list of BinaryNum and an 'Int' represting a starting index and finds the
-- most and least common bit for the index and every index lower and concats then to form a pair of 'BinaryNum's
-- where the first is all most common bits and the second is all least common bits
getCommonBits :: [BinaryNum] -> Int -> (BinaryNum, BinaryNum)
getCommonBits nums 0 = ([mcb], [lcb])
  where (mcb, lcb) = getCommonBitsAt nums 0
getCommonBits nums i = (mcb : gamma, lcb : epsilon)
  where (gamma, epsilon) = getCommonBits nums (i - 1)
        (mcb, lcb) = getCommonBitsAt nums i

-- | 'BitCriteria' is a function that takes in the amount of ones and zeros in a 'BinaryNum' and
-- returns a 'BinaryDigit' based on the counts
type BitCriteria = (Int -> Int -> BinaryDigit)

-- | 'co2Criteria' is a 'BitCriteria' where if the amount of ones is larger than the
-- amount of zero's return 'Zero' otherwise 'One'
co2Criteria :: BitCriteria
co2Criteria ones zeros
  | ones < zeros = One
  | otherwise = Zero

-- | 'oxygenCriteria' is a 'BitCriteria' where if the amount of zeros is larger than the
-- amount of one's return 'Zero' otherwise 'One'
oxygenCriteria :: BitCriteria
oxygenCriteria ones zeros
  | ones < zeros = Zero
  | otherwise = One

-- | 'part2' finds the co2 and oxygen rates for a list of 'BinaryNum' as per
-- https://adventofcode.com/2021/day/3#part2 and returns their product
part2 :: [BinaryNum] -> Int
part2 [] = 0
part2 input = do
  let filterFunc = filterRatings input 0
  let co2 = filterFunc co2Criteria
  let oxygen = filterFunc oxygenCriteria
  binNumToInt oxygen * binNumToInt co2

-- | 'filterRatings' performs a 'BitCritera' for every bit in the 'BinaryNum's at the index given as a 'Int'.
-- it then concats all results into a 'BinaryNum'
filterRatings ::  [BinaryNum] -> Int -> BitCriteria -> BinaryNum
filterRatings [num] _ _ = num
filterRatings [] _ _ = [] -- Should  never occur, just here to avoid inifite recursion on invalid input
filterRatings nums pos criteria = filterRatings filtered (pos + 1) criteria
  where filtered = filterMCB criteria nums pos


-- | 'filterMCB' filters a list of 'BinaryNum's and returns the ones that share the most
-- common bit at the position given by the 'Int' (zero indexed) 
filterMCB :: BitCriteria -> [BinaryNum] -> Int -> [BinaryNum]
filterMCB criteria nums pos = filter filterBit nums
  where bits = map (!! pos) nums
        ones = length $ filter (== One) bits
        zeros = length $ filter (== Zero) bits
        filterBit num = (num !! pos) == criteria ones zeros


