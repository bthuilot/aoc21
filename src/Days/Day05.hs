{-# LANGUAGE OverloadedStrings #-}

module Days.Day05
  ( runDay05
  ) where

import Results
import Utils
import System.IO.Unsafe
import qualified Data.Text as T
import qualified Data.Text.Read as R
import Data.Either
import Data.List
import Debug.Trace

-- | 'Point' represents an X,Y point 
type Point = (Int, Int)

-- | 'Line' represents a line on the XY Plane from first 'Point' in the pair to
-- the second 'Point' in the pair
type Line = (Point, Point)

-- | 'runDay05' runs the 2 parts for Day 5 with the contents for a given file path
-- https://adventofcode.com/2021/day/5
runDay05 :: String -> Result
runDay05 filename = do
  let input = unsafePerformIO $ readFileLinesText filename
  let graphedLines = sort $ parseInput input
  let (p1Points, p2Points) = getSortedPartPoints graphedLines
  let p1 = length $ filter (\x -> length x > 1) $ group p1Points
  let p2 = length $ filter (\x -> length x > 1) $ group p2Points
  (Just $ show p1, Just $ show p2)

{- Input parsing functions -}

-- | 'parseInput' parses 'T.Text' into a list of 'Line'
-- as per the spec https://adventofcode.com/2021/day/5
parseInput :: [T.Text] -> [Line]
parseInput = map parseLine


-- | 'parseLine' parses a single line of 'T.Text' into a 'Line'
parseLine :: T.Text -> Line
parseLine line = (parsePoint p1, parsePoint p2)
  where (p1, _) = T.breakOn " -> " line
        (_, p2) = T.breakOnEnd " -> " line

-- | 'parsesPoint' parses a 'T.Text' of the form "x,y" into a 'Point' of
-- x and y as 'Int'
parsePoint :: T.Text -> Point
parsePoint point = (parsedX, parsedY)
  where (x, _) = T.breakOn "," point
        (_, y) = T.breakOnEnd "," point
        (parsedX, _) = fromRight (0, "") $ R.decimal x
        (parsedY, _)  = fromRight (0, "") $ R.decimal y


{- Part 1 & 2 logic functions -}

-- | 'getSortedPoints' gets a list of sorted 'Point' for part 1 and and part 2
getSortedPartPoints :: [Line] -> ([Point], [Point])
getSortedPartPoints ls = (nonDiagP, allPoints) -- part 1 is only non diagonal and part 2 is all points
  where (diag, nonDiag) = partition isDiag ls
        diagP = sort $ foldl (\acc l -> acc ++ listPoints l) [] diag -- create points from diagonal lines
        nonDiagP = sort $ foldl (\acc l -> acc ++ listPoints l) [] nonDiag -- create points from horizontal/vertical
        allPoints = mergePoints diagP nonDiagP 

-- | 'mergePoints' merges a 2 lists of 'Point' that is assumed to be pre-sorted
mergePoints ::[Point] -> [Point] -> [Point]
mergePoints x [] = x
mergePoints [] y = y
mergePoints (x:xs) (y:ys) = if x > y
                            then y : (mergePoints (x:xs) ys)
                            else x : (mergePoints xs (y:ys))

-- | 'isDiag' returns true if the line is a diagonal
isDiag :: Line -> Bool
isDiag ((x1, y1), (x2, y2)) = x1 /= x2 &&  y1 /= y2 

-- | 'listPoints' turns a 'Line' into a list of 'Point' that the line passes through
listPoints :: Line -> [Point]
listPoints ((x1, y1), (x2, y2))
  | x1 == x2 && y1 == y2 = [(x1, y1)]
  | otherwise = point : (listPoints ((newX1, newY1), (x2, y2)))
  where point = (x1, y1)
        newX1 = findNextPoint x1 x2
        newY1 = findNextPoint y1 y2
        
-- | 'findNextPoint' takes in a 1 dimensional point as an 'Int' and the ending point 'Int' and returns what
-- the next point in the sequnce would be
findNextPoint :: Int -> Int -> Int
findNextPoint cur end 
  | cur > end = cur - 1
  | cur < end = cur + 1
  | otherwise = cur


