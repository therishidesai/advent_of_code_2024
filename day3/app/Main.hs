module Main where

import Text.Read
import Text.Regex.Posix

mulRegex :: String
mulRegex = "mul\\([[:space:]]*-?[0-9]+\\.?[0-9]*[[:space:]]*,[[:space:]]*-?[0-9]+\\.?[0-9]*[[:space:]]*\\)"

mulDoDontRegex :: String
mulDoDontRegex = "mul\\([[:space:]]*-?[0-9]+\\.?[0-9]*[[:space:]]*,[[:space:]]*-?[0-9]+\\.?[0-9]*[[:space:]]*\\)|(do\\([[:space:]]*\\))|(don't\\([[:space:]]*\\))"

findMuls :: String -> [String]
findMuls s = getAllTextMatches $ (s =~ mulRegex)

checkDont :: String -> Bool
checkDont s = let m = s =~ "don't\\([[:space:]]*\\)" :: String
  in if length m > 0 then True
  else False

checkDo :: String -> Bool
checkDo s = let m = s =~ "do\\([[:space:]]*\\)" :: String
  in if length m > 0 then True
  else False

doDontFilter :: [String] -> [String] -> Bool -> [String]
doDontFilter (x:xs) l f = if checkDont x then doDontFilter xs l False
                          else if checkDo x then doDontFilter xs l True
                          else if f then doDontFilter xs (l ++ [x]) True
                          else doDontFilter xs l False
doDontFilter [] l _ = l

findMulsDoDont :: String -> [String]
findMulsDoDont s = getAllTextMatches $ (s =~ mulDoDontRegex)

extractNums :: String -> (Maybe Int, Maybe Int)
extractNums s = let matches = s =~ "mul\\((-?[0-9]+),(-?[0-9]+)\\)"
    in case matches of
        (_:x:y:_):_ -> (readMaybe x, readMaybe y)
        _           -> (Nothing, Nothing)

doMul :: (Maybe Int, Maybe Int) -> Int 
doMul (Just x, Just y) = x * y
doMul (_, _) = 0

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ sum (doMul <$> (extractNums <$> (findMuls contents)))
  print $ sum (doMul <$> (extractNums <$> (doDontFilter (findMulsDoDont contents) [] True)))
