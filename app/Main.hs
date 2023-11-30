module Main where

import Data.Time
import App.DayX

timeFunction :: IO () -> IO ()
timeFunction function = do
  startTime <- getCurrentTime
  function
  endTime <- getCurrentTime
  let diff = diffUTCTime endTime startTime
  putStrLn $ " in: " ++ show (diffToMs diff)
  where
    diffToMs :: NominalDiffTime -> String
    diffToMs t = init (show $ t * 10 ^ 3) ++ " ms"

-- ghc --make Main.hs -hidir hiFiles -odir oFiles
main = do
  input <- readFile "inputFiles/inputDayX.txt"
  timeFunction $ putStr $ "A: " ++ (solveA . lines $ input)
  timeFunction $ putStr $ "B: " ++ (solveB . lines $ input)

-- alternately, main = print . map readInt . words =<< readFile "test.txt"