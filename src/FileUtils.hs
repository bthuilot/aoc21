
module FileUtils where

readFileLines :: String -> IO [String]
readFileLines filename = do
  contents <- readFile filename
  return $ lines contents


  
