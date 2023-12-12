module Main where

import Data.Time
import AocUtils
import DayX as DX
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12

timeFunction :: String -> IO ()
timeFunction function = do
  startTime <- getCurrentTime
  putStr function
  endTime <- getCurrentTime
  let diff = diffUTCTime endTime startTime
  putStrLn $ " in: " ++ diffToMs diff
  where
    diffToMs :: NominalDiffTime -> String
    diffToMs t = init (show $ t * 10 ^ 3) ++ " ms"

main = do
  solveDay day1
  solveDay day2
  solveDay day3
  solveDay day4
  solveDay day5
  solveDay day6
  solveDay day7
  solveDay day8
  solveDay day9
  solveDay day10
  --solveDay day11
  solveDay day12

solveDay :: AocDay -> IO ()
solveDay (MkDay i a b) = do
  putStrLn "-----------------------------------------------------------"
  input <- readFile $ "inputFiles/inputDay"++show i++".txt"
  putStrLn $ "Day " ++ show i ++ ": "
  timeFunction $ "A: " ++ (a . lines $ input)
  timeFunction $ "B: " ++ (b . lines $ input)

-- ghc --make Main.hs -hidir hiFiles -odir oFiles