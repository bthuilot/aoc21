
module Utils where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (isPrefixOf, group, sort)


{- String file reading -}

-- | 'readFileLines' reads in the contents from a given file path and splits the contents into lines
readFileLines :: String -> IO [String]
readFileLines filename = do
  contents <- readFile filename
  return $ lines contents

-- | 'readFileLinesInts' reads in the contents from a given file path and parses every line into an int
readFileLinesInts :: String -> IO [Int]
readFileLinesInts filename = do
  contents <- readFile filename
  return $ map read (lines contents)


{- Data.Text file reading -}

-- | 'readFileLinesText' reads the contents of a file, given a filepath
-- and returns a '[Data.Text]' of the lines of the file
readFileLinesText :: String -> IO [T.Text]
readFileLinesText filename = do
  contents <- TIO.readFile filename
  return $ T.lines contents

-- | 'readFileText' reads the contents of a file, from a given
-- filepath and returns it as a 'Data.Text'
readFileText :: String -> IO T.Text
readFileText = TIO.readFile 


{- Utility functions -}

-- | 'splitAt' splits a given string at the first occurence of the delimiter and returns the prefix and suffix
-- with the delimiter removed. If no occurence of the delimiter is found, the string is returned as the prefix and the
-- suffix is empty
splitAtChar :: String -> Char -> (String, String)
splitAtChar (h : str) del
  | h == del = ("", str)
  | otherwise = (h : prefix, suffix)
    where (prefix, suffix) = splitAtChar str del
splitAtChar [] _ = ("", "")

-- | 'splitAtStr' splits a given 'String' at the first occurence of the second 'String' and returns
-- a tuple of the 'String' before and after the delimiter with the delimiter removed
splitAtStr :: String -> String -> (String, String)
splitAtStr [] _ = ("", "")
splitAtStr str@(c : xs) del
  | del `isPrefixOf` str = ("", drop (length del) str)
  | otherwise = (c : prefix, suffix)
    where
      (prefix, suffix) = splitAtStr xs del
      
-- | 'splitStr' splits a 'String' at all occurences of the given 'Char' and returns
-- a '[String]' of the split string with all occurences of 'Char'
splitStr :: String -> Char -> [String]
splitStr [] _ = []
splitStr s c
  | before == s = [before]
  | before == "" = splitStr after c
  | otherwise = before : splitStr after c
  where before = takeWhile (/= c) s
        (_: after) = dropWhile (/= c) s

-- | 'removeDupes' removes all duplicated items from the list
-- and returns the list sorted
removeDupes :: (Ord a) => [a] -> [a]
removeDupes = map head . group . sort
