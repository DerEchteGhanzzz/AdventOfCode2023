module Main where

  import DayX
  import Data.Time
  
  timeFunction :: IO () -> IO ()
  timeFunction function = do
   startTime <- getCurrentTime
   function
   endTime <- getCurrentTime
   let diff = diffUTCTime endTime startTime
   putStrLn $ "Execution Time: " ++ show (diffToMs diff)
   where
    diffToMs :: NominalDiffTime -> String
    diffToMs t = init (show $ t * 10^3) ++ " ms"

  -- ghc --make Main.hs -hidir hiFiles -odir oFiles
  main = do
          input <- readFile "inputFiles/day0.txt"
          timeFunction $ putStrLn . solveA . lines $ input
          timeFunction $ putStrLn . solveB . lines $ input

  -- alternately, main = print . map readInt . words =<< readFile "test.txt"