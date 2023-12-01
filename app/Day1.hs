module Day1 (day1) where
import AocUtils
import Data.Char
import Data.List.Utils (replace)

day1 :: AocDay
day1 = MkDay 1 solveA solveB

solveA :: [String] -> String
solveA input = show $ foldr (\x i -> 
  let n = filter isDigit x
  in i + read [head n, last n]) 0 input

solveB :: [String] -> String
solveB = solveA . map textNumsToDigits

textNumsToDigits :: String -> String
textNumsToDigits str = foldr (\(s, r) acc -> replace s (s++r++s) acc) str numbersMap

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