
module Utils where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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

-- | 'splitAt' splits a given string at the first occurence of the delimiter and returns the prefix and suffix
-- with the delimiter removed. If no occurence of the delimiter is found, the string is returned as the prefix and the
-- suffix is empty
splitAtChar :: String -> Char -> (String, String)
splitAtChar (h : str) del
  | h == del = ("", str)
  | otherwise = (h : prefix, suffix)
    where (prefix, suffix) = splitAtChar str del
splitAtChar [] _ = ("", "")


readFileLinesText :: String -> IO [T.Text]
readFileLinesText filename = do
  contents <- TIO.readFile filename
  return $ T.lines contents

readFileText :: String -> IO T.Text
readFileText = TIO.readFile 
