import Results
import Day
import Control.Monad
import System.Exit

-- | 'TestStats' represent the statistics of running a test suite,
-- the amount of passed, skipped and failing tests
type TestStats = (Int, Int, Int)

-- | 'TestResults' represent the results of running a test suite,
-- the amount of tests run and the 'TestStats' of the run
type TestResults = (Int, TestStats)

-- | 'Test' represents a test to run by the test suite
-- it contains a 'Day' repesenting which day, 'String' of an input file path,
-- and a 'Result' of what is expected
data Test = Test Day String Result

-- | 'testInputDirectory' is the directory where the test input text files are located. Path is
-- relative to root of repository
testInputDirectory :: String
testInputDirectory = "./assets/testinputs/"

-- | 'test' is an Array of all 'Test's to run
tests :: [Test]
tests = [
  (Test D01 "day01.txt" (Just "7", Just "5")),
  (Test D02 "day02.txt" (Just "150", Just "900")),
  (Test D03 "day03.txt" (Just "198", Just "230")),
  (Test D04 "day04.txt" (Just "4512", Just "1924")),
  (Test D05 "day05.txt" (Just "5", Just "12"))
  ]

-- | 'runTest' takes in the exisiting 'TestResults' and a Test and returns an update 'TestResults'.
-- Function is used to fold over all 'Test's
runTest :: TestResults -> Test -> IO TestResults
runTest (t, (p, f, s)) (Test d file (expected1, expected2)) = do
  _ <- putStrLn $ "Running " ++ show d
  let (output1, output2) = runDay d (testInputDirectory ++ file)
  putStr $ "part 1: "
  (p1, f1, s1) <-  compareAndPrintResult expected1 output1
  putStr $ "part 2: "
  (p2, f2, s2) <- compareAndPrintResult expected2 output2
  putStrLn $ "" -- For spacing
  return $ (t + 2, (p + p1 + p2, f + f1 + f2, s + s1 + s2))

-- | 'compareAndPrintResult' compares two 'Maybe String' representing the expected and actual output of a 'Day's part and prints
-- the result. The test is considered passing if the expected is equal to the actual, and failing otherwise.
-- If the expected is 'Nothing' the test is considered skipped. If the expected is 'Nothing' the test is considered failed
-- (since the part is not defined yet, and the expected is not 'Nothing')
compareAndPrintResult :: Maybe String -> Maybe String -> IO TestStats
compareAndPrintResult Nothing _ = do
  _ <- putStrLn "skip..."
  return $ (0,0,1)
compareAndPrintResult _ Nothing = do
  _ <- putStrLn  $ "not defined, " ++ "\x1b[31m" ++ "fail." ++ "\x1b[0m"
  return $ (0,1,0)
compareAndPrintResult (Just expected) (Just actual)
  | expected == actual = do
      _ <- putStrLn $ "\x1b[32m" ++ "pass!" ++ "\x1b[0m"
      return $ (1, 0, 0)
  | otherwise = do
      _ <- putStrLn $ "\x1b[31m" ++ "fail:" ++ "\x1b[0m expected " ++ expected ++ " but got " ++ actual
      return $ (0, 1, 0)

-- | 'printTestResults' prints the total amount of tests, and the amount of passing, failing and skipped
-- from a 'TestResults'
printTestResults :: TestResults -> IO ()
printTestResults (t, (p, f, s)) = do
  _ <- putStrLn $ "Ran " ++ show t ++ " tests"
  _ <- putStrLn $ show p ++ " passed"
  _ <- putStrLn $ show f ++ " failed"
  putStrLn $ show s ++ " skipped"
  
main :: IO ()
main = do
  results@(_, (_, f, _)) <- foldM runTest (0, (0,0,0)) tests
  printTestResults results 
  if f > 0
    then exitWith (ExitFailure 1)
    else exitWith ExitSuccess
