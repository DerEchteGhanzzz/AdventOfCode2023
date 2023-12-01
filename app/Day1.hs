module Day1 where
import Data.Char
import Data.List.Utils (replace)

solveA :: [String] -> String
solveA input = show $ foldr (\x i -> 
  let n = filter isDigit x 
  in i + fstlstNum n) 0 input
  where
    fstlstNum str = read [head str, last str]

solveB :: [String] -> String
solveB = solveA . map textNumsToDigits

textNumsToDigits :: String -> String
textNumsToDigits str = foldr (\(s, r) acc ->  replace s (s++r++s) acc) str numbersMap

numbersMap :: [(String, String)]
numbersMap =  [ ("one", "1")
              , ("two", "2")
              , ("three", "3")
              , ("four", "4")
              , ("five", "5")
              , ("six", "6")
              , ("seven", "7")
              , ("eight", "8")
              , ("nine", "9")
              ]