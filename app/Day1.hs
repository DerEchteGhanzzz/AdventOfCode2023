module Day1 where
import Data.Char
import Data.List.Utils (replace)

solveA :: [String] -> String
solveA input = show $ foldr (\x i -> 
  let n = filter isDigit x 
  in i + fstlst n) 0 input

solveB :: [String] -> String
solveB input = show $ foldr (\x acc -> 
  let ns = filterNumbers x
  in acc + fstlst ns) 0 input

fstlst :: String -> Int
fstlst xs = read [head xs, last xs]

filterNumbers :: String -> String
filterNumbers str = filter isDigit $ foldr (\(s, r) acc -> replace s r acc) str numbersMap

numbersMap :: [(String, String)]
numbersMap =  [ ("one", "o1n")
              , ("two", "t2o")
              , ("three", "t3e")
              , ("four", "f4r")
              , ("five", "f5e")
              , ("six", "s6x")
              , ("seven", "s7n")
              , ("eight", "e8t")
              , ("nine", "n9e")
              ]