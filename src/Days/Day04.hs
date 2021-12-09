{-# LANGUAGE OverloadedStrings #-}

module Days.Day04
  ( runDay04
  ) where

import Results
import Utils
import System.IO.Unsafe
import qualified Data.Text as T
import qualified Data.Text.Read as R
import Data.Either
import Data.List

-- | 'BoardNum' represents a number on a bingo 'Board', it contains an 'Int' value and a 'Bool' of whether it is currently
-- marked or not
type BoardNum = (Int, Bool)

-- | 'Board' represents a bingo board, it is a 2d array of 'BoardNum'
type Board = [[BoardNum]]

-- | 'CalledNumbers' is a list of numbers called during the bingo game, in the order they are called
type CalledNumbers = [Int]

-- | A 'BingoGame' represents a game of bingo, with a list of numbers in the order they are called
-- and a list of boards that are being played
data BingoGame = Bingo CalledNumbers [Board] -- | ^ 'Bingo' is the contructor for the 'BingoGame'


-- | 'runDay04' runs the 4 parts for Day 4 with the contents for a given file path
-- https://adventofcode.com/2021/day/4
runDay04 :: String -> Result
runDay04 filename = do
  let input = unsafePerformIO $ readFileLinesText filename
  let bingo = parseInput input
  let scores = findScores bingo
  let winner = head scores
  let loser = last scores
  (Just $ show winner, Just $ show loser)

-- | 'parseInput' parses lines of 'T.Text' into a 'BingoGame'
-- as per the spec from https://adventofcode.com/2021/day/4
parseInput :: [T.Text] -> BingoGame
parseInput [] = Bingo [] []
parseInput input = Bingo (parseNumbers calledNumInput) boards
  where
    calledNumInput = head input
    boards = parseBoards (dropWhile (== "") (tail input))

-- | 'parseNumbers' parses the list of numbers called in a bingo game (represented as a 'T.Text' string of numbers
-- seperated by ",") into a 'CalledNumbers'
parseNumbers :: T.Text -> CalledNumbers
parseNumbers t = [i | (i, _) <- rights $ map R.decimal splitInput ]
  where splitInput = T.splitOn "," t

-- | 'parseBoards' parses a list of boards into a list of 'Board', with each board seperated by a empty 'T.Text'
parseBoards :: [T.Text] -> [Board]
parseBoards [] = []
parseBoards input
  | null board  = parseBoards (dropWhile (=="") input)
  | otherwise = parseBoard board : parseBoards boardRemoved
  where
    board = takeWhile (/= "") input
    boardRemoved = dropWhile (/= "") input
        
-- | 'parseBoard' parses an individual board, represent as 'T.Text' lines of numbers seperated by spaces
parseBoard :: [T.Text] -> Board
parseBoard = map parseRow
  where
    parseRow rowInput = [(i, False) | (i, _) <- rights $ map R.decimal (T.splitOn " " rowInput)]

-- | 'findScores' takes a 'BingoGame' and runs the game, returing a list of scores from the game, in the order the
-- games finished
findScores :: BingoGame -> [Int]
findScores (Bingo [] _) = []
findScores (Bingo (i : xs) boards) = map (\w -> i * calculateScore w) winners ++ findScores (Bingo xs remaining)
  where
    markedBoards = markBoards boards i
    (winners, remaining) = partition isWinner markedBoards

-- | 'markBoards' takes in a list of 'Board' and a 'Int' and for each board, marks the space with that integer
-- and returns the marked boards
markBoards :: [Board] -> Int -> [Board]
markBoards boards i = map (markBoard i) boards

-- | 'markBoard' takes in an 'Int', and a 'Board' and marks that integers spot on the board and returns the new board
markBoard :: Int -> Board -> Board          
markBoard i = map rowFunc
  where
    numFunc (num, m) = (num, m || num == i)
    rowFunc = map numFunc
        
-- | 'calculateScore' takes in a 'Board' and calculates it's score, defined by the total of all non marked numbers
calculateScore :: Board -> Int
calculateScore = unmarkedTotal 
  where
    unmarkedRowTotal = foldl (\acc (i, m) -> acc + if m then 0 else i) 0
    unmarkedTotal = foldl (\acc row -> acc + unmarkedRowTotal row) 0 

-- | 'isWinner' determines if a 'Board' has won, defined as having a row or col of marked numbers
isWinner :: Board -> Bool
isWinner [] = False
isWinner b = rowWon || colWon
  where
    rowMarked = all snd
    rowWon = any rowMarked b
    colMarked index = all (snd . (!! index)) b
    len = length $ head b
    colWon = any colMarked [0..(len - 1)]
