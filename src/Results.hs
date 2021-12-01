module Results
  (showResults, Result) where

type Result = (Maybe String, Maybe String)


-- | 'showResults' returns a formated string of a given 'Result' and a name for the results
showResults :: String -> Result -> String
showResults name (out1, out2) = do
      let underline = underlineStr name
      name ++ "\n" ++ underline ++ "\n" ++ (showPart 1 out1) ++ (showPart 2 out2)


-- | 'underlineStr' creates an underline for a string by repeating
-- | the character "=" for the length of the string
underlineStr :: String -> String
underlineStr = flip replicate '=' . length


-- | 'showPart' returns a formatted string of a day part's results along with the number of which part it is
showPart :: Integer -> Maybe String -> String
showPart partNum Nothing = "part " ++ show partNum ++ ": Not implemented\n"
showPart partNum (Just s) = "part " ++ show partNum ++ ": " ++ s ++ "\n"
