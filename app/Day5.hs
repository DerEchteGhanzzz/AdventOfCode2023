module Day5 (day5) where

import AocUtils
import Data.List.Split
import Data.Maybe

day5 :: AocDay
day5 = MkDay 5 solveA solveB

solveA :: [String] -> String
solveA input = (show . getBegin . minimum) (foldr ((\s acc -> seedsToLocation maps [s] ++ acc) . (`Range` 1)) [] seeds)
  where
    (seeds, maps) = parseInput input

solveB :: [String] -> String
solveB input = show . getBegin . minimum . foldr (\s acc -> seedsToLocation maps [s] ++ acc) [] $ toRanges seeds
  where
    (seeds, maps) = parseInput input

seedsToLocation :: [[(Int, Int, Int)]] -> [Range] -> [Range]
seedsToLocation ms rs
  = foldl
      (\ rs' m -> foldr (\ r acc -> insertRange m r ++ acc) [] rs') rs ms

toRanges :: [Int] -> [Range]
toRanges [] = []
toRanges (b:e:rest) = Range b e : toRanges rest

insertRange :: [(Int, Int, Int)] -> Range -> [Range]
insertRange rest (Range x 0) = []
insertRange [] r             = [r]
insertRange ((d, s, mapLen):rest) r1@(Range b bLen) | not (hasOverlap r1 mapRange) = insertRange rest r1
                                                    | r1 `inside` mapRange = [mapFromSToD r1 s d]
                                                    | mapRange `inside` r1 = insertRange rest (Range b (s-b)) ++ [mapFromSToD (Range s mapLen) s d] ++ insertRange rest (Range (s+mapLen) (b+bLen-s-mapLen))
                                                    | b < s = mapFromSToD (snd bSmall) s d : insertRange rest (fst bSmall)
                                                    | b > s = mapFromSToD (fst bBig) s d : insertRange rest (snd bBig)
                                                    | otherwise = error "range not covered"
                                                    where
                                                      bSmall = overlapL r1 mapRange
                                                      bBig = overlapR mapRange r1
                                                      mapRange = Range s mapLen

mapFromSToD :: Range -> Int -> Int -> Range
mapFromSToD (Range b e) s d = Range (abs (s-b) + d) e

inside :: Range -> Range -> Bool
inside r1@(Range b bLen) r2@(Range s sLen) = s <= b && b <= s+sLen && s <= b+bLen && b+bLen <= s+sLen

overlapL :: Range -> Range -> (Range, Range)
overlapL r1@(Range b bLen) r2@(Range s sLen) = (Range b (s-b), Range s (b+bLen-s))

overlapR :: Range -> Range -> (Range, Range)
overlapR r1@(Range leftStart leftLen) r2@(Range rightStart rightLen) = (Range rightStart (leftStart+leftLen-rightStart), Range (leftStart+leftLen) (rightLen+rightStart-(leftStart+leftLen)))

hasOverlap :: Range -> Range -> Bool
hasOverlap r1@(Range b bLen) r2@(Range s sLen) = b <= s && b+bLen > s || s <= b && s+sLen > b

data Range = Range {getBegin :: Int, getLength :: Int}

start :: Range -> Int
start (Range a _) = a

end :: Range -> Int
end (Range a b) = a + b

instance Eq Range where
  (Range b e) == (Range s l) = b == s

instance Ord Range where
  (Range b e) <= (Range s l) = b <= s

instance Show Range where
  show (Range b l) = show b ++ " -> " ++ show (b+l-1)

parseInput :: [String] -> ([Int], [[(Int, Int, Int)]])
parseInput input = (parseSeeds s, map parseMap m)
  where
    (s : m) = splitOn [""] input

parseMap :: [String] -> [(Int, Int, Int)]
parseMap (_ : numbers) = map ((\[d, s, l] -> (read d, read s, read l)) . words) numbers

parseSeeds :: [String] -> [Int]
parseSeeds [line] = map read . tail . words $ line
parseSeeds x      = error $ show x ++ " is invalid input"