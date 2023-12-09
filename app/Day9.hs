module Day9 (day9) where
import AocUtils

day9 :: AocDay
day9 = MkDay 9 solveA solveB

extrapolate :: [[Int]] -> Int
extrapolate [range] = last range
extrapolate (r:rs)  = last r + extrapolate rs
extrapolate _       = error "no empty ranges allowed"

differentiate :: [Int] -> [[Int]]
differentiate range | all (==0) range = []
                    | otherwise = range : differentiate differences
                    where
                      differences = snd $ foldl (\(prev, l) next -> (next, l ++ [next - prev])) (head range, []) (tail range) 

solveA :: [String] -> String
solveA = show . sum . map (extrapolate . differentiate) . parseInput

solveB :: [String] -> String
solveB = show . sum . map (extrapolate . differentiate . reverse) . parseInput

parseInput :: [String] -> [[Int]]
parseInput = map (map read . words)