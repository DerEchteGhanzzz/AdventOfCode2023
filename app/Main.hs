module Main where

import Data.Time
import DayX as DX
import Day1
import Day2

timeFunction :: String -> IO ()
timeFunction function = do
  startTime <- getCurrentTime
  putStr function
  endTime <- getCurrentTime
  let diff = diffUTCTime endTime startTime
  putStrLn $ " in: " ++ show (diffToMs diff)
  where
    diffToMs :: NominalDiffTime -> String
    diffToMs t = init (show $ t * 10 ^ 3) ++ " ms"

-- ghc --make Main.hs -hidir hiFiles -odir oFiles
main = do
  solveDay 1 Day1.solveA Day1.solveB
  solveDay 2 Day2.solveA Day2.solveB

type Solver = [String] -> String

solveDay :: Int -> Solver -> Solver -> IO ()
solveDay i a b = do
  putStrLn "-----------------------------------------------------------"
  input <- readFile $ "inputFiles/inputDay"++show i++".txt"
  putStrLn $ "Day " ++ show i ++ ": "
  timeFunction $ putStr $ "A: " ++ (a . lines $ input)
  timeFunction $ putStr $ "B: " ++ (b . lines $ input)

-- alternately, main = print . map readInt . words =<< readFile "test.txt"