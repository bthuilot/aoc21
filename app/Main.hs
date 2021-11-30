module Main where

import Day


days :: [(Day, String)]
days = [(D01, "input 1"), (D02, "input")]

main :: IO ()
main = do
  let output = map (\(d, f) -> (runDay d f)) days
  putStrLn $ foldl (++) "" output 


