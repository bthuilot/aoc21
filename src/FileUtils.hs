
module FileUtils where

readFileLines :: String -> IO [String]
readFileLines filename = do
  contents <- readFile filename
  return $ lines contents

readFileLinesInts :: String -> IO [Int]
readFileLinesInts filename = do
  contents <- readFile filename
  return $ map read (lines contents)
  
