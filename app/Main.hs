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
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25

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
  -- solveDay day9
  -- solveDay day10
  --solveDay day11
  -- solveDay day12
  solveDay day13
  -- solveDay day14
  solveDay day15
  -- solveDay day16
  -- solveDay day17
  solveDay day18
  solveDay day19
  -- solveDay day20
  solveDay day21
  -- solveDay day22
  -- solveDay day23
  solveDay day24
  solveDay day25

solveDay :: AocDay -> IO ()
solveDay (MkDay i a b) = do
  putStrLn "-----------------------------------------------------------"
  input <- readFile $ "inputFiles/inputDay"++show i++".txt"
  putStrLn $ "Day " ++ show i ++ ": "
  timeFunction $ "A: " ++ (a . lines $ input)
  timeFunction $ "B: " ++ (b . lines $ input)

-- ghc --make Main.hs -hidir hiFiles -odir oFiles