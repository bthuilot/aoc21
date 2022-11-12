module Days.Day10 (
  runDay10
  ) where

import Results
import Utils
import System.IO.Unsafe


-- | 'runDay01' will run Day 1 of AdventOfCode with the contents from the given file name
runDay10 :: String -> Result
runDay10 filename = do
  let lines = unsafePerformIO $ readFileLines filename
  let parsedLines = map (parseLine []) lines
  let part1Result = part1 [Corrupt c | (Corrupt c) <- parsedLines]
  let part2Result = Nothing 
  (Just $ show part1Result, Nothing)

-- | 'CharPair' represents the different types of open and closing character pairs
data CharPair = Paren | Bracket | Brace | Carrot deriving Eq

-- | 'pairScore' maps a 'CharPair' to their respecitve score
pairScore :: CharPair -> Int
pairScore Paren = 3
pairScore Bracket = 57
pairScore Brace = 1197
pairScore Carrot = 25137

-- | 'CorruptCharCount' represents the counts of each type of 'CharPair' for a string
data CorruptCharCount = CorruptCount { paren :: Int,
                                       bracket :: Int,
                                       brace :: Int,
                                       carrot :: Int
                                     } deriving (Show)

-- | 'Syntax' represents the different types of snytax a list can evalualte to
data Syntax = Valid -- ^ 'Valid' represents a line with the correct open and closing characters
  | Invalid -- ^ 'Invalid' repsresents a line that contains non 'CharPair' characters
  | Incomplete [CharPair] -- ^ 'Incomplete' represents a line that ended with open characters
  | Corrupt CharPair -- ^ 'Corrupt' reperesents a line that inclosed with an incorrect CharPair

-- | 'emptyCharCount' reperesents the initial empty character count 
emptyCharCount :: CorruptCharCount
emptyCharCount = CorruptCount{paren=0,bracket=0,brace=0,carrot=0}

-- | 'calculateCountScore' calculates the score of all the corrupt closing characters found in all lines
calculateCountScore :: CorruptCharCount -> Int
calculateCountScore (CorruptCount paren bracket brace carrot) =
  parenScore + bracketScore + braceScore + carrotScore
  where
    parenScore = paren * pairScore Paren
    braceScore = brace * pairScore Brace
    bracketScore = bracket * pairScore Bracket
    carrotScore = carrot * pairScore Carrot

-- | 'updateCount' updates the current corrupt char count based on the syntax of the line
updateCount :: CorruptCharCount -> Syntax -> CorruptCharCount
updateCount c (Corrupt chr)
  | chr == Paren = c{paren=paren c + 1}
  | chr == Bracket = c{bracket=bracket c + 1}
  | chr == Brace = c{brace=brace c + 1}
  | chr == Carrot = c{carrot=carrot c + 1}
updateCount c _ = c

-- | 'part1' will determine the score of each corrupt line.
-- | a corrupt line is defined by a line that closes with the wrong character,
-- | and the score for each is determined by the incorrect character used
part1 :: [Syntax] -> Int
part1 lines = calculateCountScore total
  where
      total = foldl updateCount emptyCharCount lines
    

-- | 'ParsedChar' represents a character parsed into if its an open or close
-- | and what 'CharPair' it belongs to, or if its 'Unknown'
data ParsedChar = OpenChar CharPair | CloseChar CharPair | Unknown

-- | 'parseChar' converts a character into if its a open or close char and what 'CharPair'
-- | it belongs to.
parseChar :: Char -> ParsedChar
parseChar '}' = CloseChar Brace
parseChar '{' = OpenChar Brace
parseChar ']' = CloseChar Bracket
parseChar '[' = OpenChar Bracket
parseChar ')' = CloseChar Paren
parseChar '(' = OpenChar Paren
parseChar '<' = OpenChar Carrot
parseChar '>' = CloseChar Carrot
parseChar _ = Unknown

-- | 'parseLine' will parse the line into a 'Syntax'
parseLine :: [CharPair] -> [Char] -> Syntax
parseLine [] [] = Valid
parseLine (x : _) [] = Incomplete [x]
parseLine stack (c : xs) =
  case parseChar c of
    OpenChar p -> parseLine (p : stack) xs
    CloseChar p -> closeChunk stack p xs
    Unknown -> Invalid

closeChunk :: [CharPair] -> CharPair -> [Char] -> Syntax
closeChunk [] p _ = Corrupt p
closeChunk (stackTop : stack) p xs
  | stackTop == p = parseLine stack xs
  | otherwise = Corrupt p
