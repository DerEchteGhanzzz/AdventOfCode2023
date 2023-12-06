module Day6 (day6) where
import AocUtils

import Debug.Trace
import Data.List.Utils (replace)
import Data.List.Split (splitOn)

day6 :: AocDay
day6 = MkDay 6 solveA solveB

type Time = Float
type Distance = Float

data Race = MkRace Time Distance
  deriving (Show, Eq)

solveA :: [String] -> String
solveA = show . product . map winPermutations . parseInput

solveB :: [String] -> String
solveB = show . product . map winPermutations . parseInput'

winPermutations :: Race -> Int
winPermutations (MkRace t bestDist) = floor $ upper - lower
  where
    disc = (t^2) - (4 * bestDist)
    (lower, upper) = ((t - sqrt disc) / 2, (t + sqrt disc) / 2)

parseInput :: [String] -> [Race]
parseInput [times, distances] = zipWith MkRace (readNumbers times) (readNumbers distances)
  where
    readNumbers = map read . tail . words
parseInput _ = error "invalid input"

parseInput' :: [String] -> [Race]
parseInput' [times, distances] = zipWith MkRace (readNumbers times) (readNumbers distances)
  where
    readNumbers = map read . tail . splitOn ":" . replace " " ""
parseInput' _ = error "invalid input"