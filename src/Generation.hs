module Generation where

indent :: Int -> String -> String
indent n s = replicate (n * 4) ' ' ++ s

blankLine :: String
blankLine = ""
