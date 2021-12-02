import Results
import Day
import Control.Monad
import System.Exit

type TestStats = (Int, Int, Int)
type TestResult = (Int, TestStats)

-- | 'Test' represents a test to run by the test suite
-- it contains a 'Day' repesenting which day, 'String' of an input file path,
-- and a 'Result' of what is expected
data Test = Test Day String Result

testInputDirectory :: String
testInputDirectory = "assets/testinputs/"

tests :: [Test]
tests = [
  (Test D01 "day01.txt" (Just "7", Just "5")),
  (Test D02 "day02.txt" (Just "150", Just "900"))
  ]

runTest :: TestResult -> Test -> IO TestResult
runTest (t, (p, f, s)) (Test d file (expected1, expected2)) = do
  _ <- putStrLn $ "Running " ++ show d
  let (output1, output2) = runDay d (testInputDirectory ++ file)
  putStr $ "part 1: "
  (p1, f1, s1) <-  compareAndPrintResult expected1 output1
  putStr $ "part 2: "
  (p2, f2, s2) <- compareAndPrintResult expected2 output2
  putStrLn $ "" -- For spacing
  return $ (t + 2, (p + p1 + p2, f + f1 + f2, s + s1 + s2))

compareAndPrintResult :: Maybe String -> Maybe String -> IO TestStats
compareAndPrintResult Nothing _ = do
  _ <- putStrLn "skip..."
  return $ (0,0,1)
compareAndPrintResult _ Nothing = do
  _ <- putStrLn "not defined, skipping"
  return $ (0,0,1)
compareAndPrintResult (Just expected) (Just actual)
  | expected == actual = do
      _ <- putStrLn $ "\x1b[32m" ++ "pass!" ++ "\x1b[0m"
      return $ (1, 0, 0)
  | otherwise = do
      _ <- putStrLn $ "\x1b[31m" ++ "fail." ++ "\x1b[0m"
      return $ (0, 1, 0)


printResults :: TestResult -> IO ()
printResults (t, (p, f, s)) = do
  _ <- putStrLn $ "Ran " ++ show t ++ " tests"
  _ <- putStrLn $ show p ++ " passed"
  _ <- putStrLn $ show f ++ " failed"
  putStrLn $ show s ++ " skipped"
  
main :: IO ()
main = do
  results@(_, (_, f, _)) <- foldM runTest (0, (0,0,0)) tests
  printResults results 
  if f > 0
    then exitWith (ExitFailure 1)
    else exitWith ExitSuccess
