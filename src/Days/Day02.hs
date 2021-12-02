module Days.Day02 where

import Results
import Utils
import System.IO.Unsafe

-- | 'Command' is a command for the diver, which is a string of either
-- "forward", "down" or "up" accompanied by a natural number
type Command = (String, Int)

-- | 'runDay02' runs the 2 parts for Day 2 with the contents for a given file path
-- https://adventofcode.com/2021/day/2
runDay02 :: String -> Result
runDay02 filename = do
  let input = unsafePerformIO $ readFileLines filename
  let cmds = parseLines input
  let part1Result = part1 cmds
  let part2Result = part2 cmds
  (Just $ show part1Result, Just $ show part2Result)

-- | 'parseLines' parses the lines of the input into a list of 'Command'
parseLines :: [String] -> [Command]
parseLines (l : xs) = (cmd, read amt :: Int) : parseLines xs
  where (cmd, amt) = splitAtChar l ' '
parseLines [] = []

-- | 'part1' returns the depth times distance after following the commands as
-- described in part 1 https://adventofcode.com/2021/day/2#part1
part1 :: [Command] -> Int
part1 cmds = do
  let (distance, depth) = foldl execCmdP1 (0,0) cmds
  distance * depth

-- | 'execCmd1' takes in a distance and depth and performs a command
-- returning the new distance and depth as described in part 1
execCmdP1 :: (Int, Int) -> Command -> (Int, Int)
execCmdP1 (dis, depth) (cmd, i)
  | cmd == "forward" = (dis + i, depth)
  | cmd == "down" =  (dis, depth + i)
  | cmd == "up" = (dis, depth - i)
execCmdP1 pos _ = pos -- Invalid command should not happen

-- | 'part2' returns the depth times the distance after following the commands as
-- described in part 2 https://adventofcode.com/2021/day/2#part2
part2 :: [Command] -> Int
part2 cmds = do
  let (distance, depth, _) = foldl execCmdP2 (0,0,0) cmds
  distance * depth

-- | 'execCmd2' takes in a distance, depth and aim and parforms a command
-- return the new distance, depth and aim as described in part 2
execCmdP2 :: (Int, Int, Int) -> Command -> (Int, Int, Int)
execCmdP2 (dis, depth, aim) (cmd, i)
  | cmd == "forward" = (dis + i, depth + (i * aim), aim)
  | cmd == "down" = (dis, depth, aim + i)
  | cmd == "up" =  (dis, depth, aim - i)
execCmdP2 pos _ = pos -- Invalid command should not happen

