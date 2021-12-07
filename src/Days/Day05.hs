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

-- | 'Grid' represents the XY Plane where each 'Int' represents the amount of lines that cross that point
type Grid = [[Int]]

-- | 'runDay05' runs the 2 parts for Day 5 with the contents for a given file path
-- https://adventofcode.com/2021/day/5
runDay05 :: String -> Result
runDay05 filename = do
  let input = unsafePerformIO $ readFileLinesText filename
  let graphedLines = parseInput input
  let initGrid = createGrid graphedLines
  let (diag, nonDiag) = partition isDiag graphedLines
  let (p1, grid) = countOverlap initGrid nonDiag -- Part 1: count only horizontal and vertical lines
  let (p2, _) = countOverlap grid diag -- Part 2: count additional diagonal lines
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

-- | 'createGrid' creates an initial 'Grid' of all zeros of the size nessecary to graph
-- all lines in the list of 'Line' provided
createGrid :: [Line] -> Grid
createGrid ls = [[0 | _ <- [0..maxX]] | _ <- [0..maxY]]
  where (maxX, maxY) = findMax ls

-- | 'countOverlap' graphs the list of 'Line' given onto 'Grid' and returns a pair the count of all tiles
-- that contain an overlap and the marked 'Grid'
countOverlap :: Grid -> [Line] -> (Int, Grid)
countOverlap initGrid ls  = (countGrid (> 1) grid, grid)
  where points = foldl (\acc l -> acc ++ listPoints l) [] ls
        grid = foldl markPoint initGrid points
        
-- | 'isDiag' returns true if the line is a diagonal
isDiag :: Line -> Bool
isDiag ((x1, y1), (x2, y2)) = x1 /= x2 &&  y1 /= y2 

-- | 'markPoint' marks a 'Point' on the given 'Grid' and returns a new 'Grid'
markPoint :: Grid -> Point -> Grid
markPoint g (x,y) = newG
  where (rBefore, r:rAfter) = splitAt y g
        (cBefore, n:cAfter) = splitAt x r
        newRow = cBefore ++ (n + 1 : cAfter)
        newG = rBefore ++ (newRow : rAfter)

-- | 'findMax' finds the maximum x and y values for the list of 'Line' given
findMax :: [Line] -> (Int, Int)
findMax [] = (0,0)
findMax (((x1, y1), (x2, y2)) : xs) = (newMaxX, newMaxY)
  where (maxX, maxY) = findMax xs
        newMaxX = maximum [maxX, x1, x2]
        newMaxY = maximum [maxY, y1, y2]

-- | 'countGrid' counts the amount of tiles in the 'Grid' that satifies the predicate of
-- type '(Int -> Bool)'
countGrid :: (Int -> Bool) -> Grid -> Int
countGrid predF = foldl (\acc row -> acc + countRow row) 0
  where countRow row = length $ filter predF row

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
