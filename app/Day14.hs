module Day14 (day14) where
import AocUtils
import Data.List as L
import Data.Map as M
import Data.Maybe (fromJust)
import Data.Tuple.Utils (fst3)

day14 :: AocDay
day14 = MkDay 14 solveA solveB

solveA :: [String] -> String
solveA = show . sum . L.map (tiltAndWeigh . reverse) . transpose

solveB :: [String] -> String
solveB input = show . weigh . fst3 . spinMeRound M.empty 0 ((10^9 - end) `mod` (end - begin)) $ platform
  where
    (platform, begin, end) = spinMeRound M.empty 0 (10^9) input

spinMeRound :: Map [String] Integer -> Integer -> Integer -> [String] -> ([String], Integer, Integer)
spinMeRound s count maxCount platform = if platform `M.member` s || count == maxCount then (platform, fromJust $ M.lookup platform s, count) else spinMeRound s' count' maxCount platform'
  where
    count' = count + 1
    s' = M.insert platform count s
    platform' = spin platform

weigh :: [String] -> Int
weigh = sum . L.map weigh' . transpose 

weigh' :: String -> Int
weigh' [] = 0
weigh' ('O':chs) = length chs + 1 + weigh' chs
weigh' (ch:chs) = weigh' chs

spin :: [String] -> [String]
spin = tilt90deg . tilt90deg . tilt90deg . tilt90deg

tilt90deg :: [String] -> [String]
tilt90deg = L.map (tilt . reverse) . transpose

tilt :: String -> String
tilt = tilt' [] 1 ""

tilt' :: String -> Int -> String -> String -> String
tilt' rocksFound index acc col = case col of
  [] -> acc ++ rocksFound
  ('#':chs) -> tilt' [] index' (acc ++ rocksFound ++ "#") chs
  ('.':chs) -> tilt' rocksFound index' (acc ++ ".") chs
  ('O':chs) -> tilt' ('O':rocksFound) index' acc chs
  where
    index' = index + 1

tiltAndWeigh :: String -> Int
tiltAndWeigh = tiltAndWeigh' [] 1 0

tiltAndWeigh' :: String -> Int -> Int -> String -> Int
tiltAndWeigh' rocksFound index total col = case col of
  []        -> total'
  ('#':chs) -> tiltAndWeigh' []         index' total' chs
  ('.':chs) -> tiltAndWeigh' rocksFound index' total  chs
  ('O':chs) -> tiltAndWeigh' ('O':rocksFound) index' total  chs
  where
    total' = total + sum [index - length rocksFound..index - 1]
    index' = index + 1