module Day5 (day5) where

import AocUtils
import Data.List.Split
import Data.Maybe
import Debug.Trace

day5 :: AocDay
day5 = MkDay 5 solveA solveB

solveA :: [String] -> String
solveA input = show . minimum . seedsToLocation $ (s, m)
  where
    (s, m, _) = parseInput input

solveB :: [String] -> String
solveB input = show . foldl (flip parseSeedRange) (toRanges seeds) $ literalMaps
  where
    (seeds, maps, literalMaps) = parseInput input

toRanges :: [Int] -> [Range]
toRanges [] = []
toRanges (b:e:rest) = Range b e : toRanges rest

parseSeedRange :: [(Int, Int, Int)] -> [Range] -> [Range]
parseSeedRange maps ranges = foldr (\rs scs -> insertRange rs scs) (map (\(_, s, l) -> Range s l) maps) ranges

insertRange :: Range -> [Range] -> [Range]
insertRange (Range x 0) rest = rest
insertRange r [] = [r]
insertRange r1@(Range b bLen) (r2@(Range s sLen):rest) | not (hasOverlap r1 r2) = r2 : insertRange r1 rest
                                                       | r1 `inside` r2 = r2 : rest
                                                       | r2 `inside` r1 = foldr insertRange rest [Range b (s-b), Range s sLen, Range (sLen+2) (b+bLen-(sLen+1))]
                                                       | otherwise = trace (show newRanges) foldr insertRange rest newRanges
                                                       where
                                                        newRanges = if getLength lb1 <= 0 || getLength ls1 <= 0 then [rb1, rs1] else [lb1, ls1]
                                                        (lb1, ls1, rb1, rs1) = overlap r1 r2

inside :: Range -> Range -> Bool
inside r1@(Range b bLen) r2@(Range s sLen) = s <= b && b <= s+sLen && s <= b+bLen && b+bLen <= s+sLen

overlap :: Range -> Range -> (Range, Range, Range, Range)
overlap r1@(Range b bLen) r2@(Range s sLen) = (Range b (s-b+1), Range s (b+bLen-s), Range b (b+bLen-s), Range s (s-b+1))

hasOverlap :: Range -> Range -> Bool
hasOverlap r1@(Range b bLen) r2@(Range s sLen) = (b <= s && b+bLen > s) || (s <= b && s+sLen > b)

data Range = Range {getBegin :: Int, getLength :: Int}
  deriving (Show)

instance Eq Range where
  (Range b e) == (Range s l) = b == s

instance Ord Range where
  (Range b e) <= (Range s l) = b <= s

seedsToLocation :: ([Int], [[Int -> Maybe Int]]) -> [Int]
seedsToLocation (seeds, maps) = map
      ( \seed ->
          foldl
            (\s m -> head . mapMaybe (\lambda -> lambda s) $ m)
            seed
            maps
      ) seeds

parseInput :: [String] -> ([Int], [[Int -> Maybe Int]], [[(Int, Int, Int)]])
parseInput input = (parseSeeds s, map parseMap m, map parseMap' m)
  where
    (s : m) = splitOn [""] input

parseMap' :: [String] -> [(Int, Int, Int)]
parseMap' (_ : numbers) = map ((\[d, s, l] -> (read d, read s, read l)) . words) numbers

parseSeeds :: [String] -> [Int]
parseSeeds [line] = map read . tail . words $ line
parseSeeds x      = error $ show x ++ " is invalid input"

parseMap :: [String] -> [Int -> Maybe Int]
parseMap (_ : numbers) =
  foldr
    ( ( \[d, s, l] acc ->
          let srs = read s; drs = read d; rl = read l
           in (\x -> if x >= srs && x <= srs + rl - 1 then Just $ abs (srs - x) + drs else Nothing) : acc
      )
        . words
    )
    [Just]
    numbers