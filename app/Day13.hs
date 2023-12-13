module Day13 (day13) where
import AocUtils

import Data.Map as M
import Data.List as L
import Data.List.Split

day13 :: AocDay
day13 = MkDay 13 solveA solveB

solveA :: [String] -> String
solveA = show . sum . L.map (findMirrors 0) . parseInput

solveB :: [String] -> String
solveB = show . sum . L.map (findMirrors 1) . parseInput

parseInput :: [String] -> [[String]]
parseInput = splitOn [""]

findMirrors :: Int -> [String] -> Int
findMirrors smudgesAllowed rows = findMirror smudgesAllowed [] (transpose rows) + 100 * findMirror smudgesAllowed [] rows

findMirror :: Int -> [String] -> [String] -> Int
findMirror smudges [] (r:rs) = findMirror smudges [r] rs
findMirror smudges (t:stack) (r:rs) | filterLen OneOff zipped == smudges && all (==Equals) (L.filter (/=OneOff) zipped) = length (t:stack)
                            | otherwise = findMirror smudges (r:t:stack) rs
                            where
                              filterLen c = length . L.filter (==c)
                              zipped = zipWith oneOff (t:stack) (r:rs)
                              oneOff x y
                                | (length . L.filter not $ zipWith (==) x y) == 1 = OneOff
                                | and (zipWith (==) x y) = Equals
                                | otherwise = NotEquals
findMirror _ s  [] = 0

data Trinary = Equals | OneOff | NotEquals
  deriving (Eq)