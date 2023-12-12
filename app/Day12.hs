module Day12 (day12) where
import AocUtils
import Data.List.Split (splitOn, wordsBy)
import Data.Char
import Data.Set as S (Set, empty, insert, singleton, union, map, foldr, fromList, filter)
import Data.List as L
import Data.List.Utils (replace)

day12 :: AocDay
day12 = MkDay 12 solveA solveB

type SpringRow = (String, [Int])

lpad :: Num a => Int -> [a] -> [a]
lpad m xs = replicate (m - length ys) 0 ++ ys
    where ys = take m xs

intToBin :: Int -> Int -> String
intToBin n totalLength = if n < 0 then error "no negatives" else intToBin' n ++ replicate (totalLength - l) '.'
  where
    l = length (intToBin' n)
    intToBin' 0 = []
    intToBin' n = ch : intToBin' (n `div` 2)
      where
        ch = if even n then '.' else '#'

getCombinations :: [SpringRow] -> [Int]
getCombinations = L.foldr ((:) . calcCombs) []

calcCombs :: SpringRow -> Int
calcCombs (row, ints) = ptrace ("doing " ++ row) binsToCheck
  where
    setQMarks r b = L.foldr (\ch (row', b') -> if ch == '?' then (head b' : row', drop 1 b') else (ch:row', b')) ([], b) r
    hashesNeeded = sum ints - charCount '#' row
    qmarks = charCount '?' row
    binsToCheck = combs row hashesNeeded qmarks ints ""

charCount :: Eq a => a -> [a] -> Int
charCount c = length . L.filter (==c)

stillValid :: String -> [Int] -> Int -> Bool
stillValid str ints total = if length str == total
  then
    length zipped == length ints && length zipped == length parts && all (\(s, i) -> length s == i) zipped
  else
    (length begin == 1 && all (\(s, i) -> length s == i) zipped) ||
    length parts <= length ints && all (\(s, i) -> length s == i) begin && all (\(s, i) -> length s <= i) zipped
  where
    parts = wordsBy (=='.') str
    zipped = zip parts ints
    (begin, end) = splitAt (length zipped - 1) zipped

combs :: String -> Int -> Int -> [Int] -> String -> Int
combs s ones totalLen ints acc =
  if not $ stillValid acc ints (length acc + length s)
    then 0
    else case s of
      ('?':chs) -> case (ones, totalLen) of
        (-1, _) -> 0
        (_, -1) -> 0
        (_, _) -> combs chs ones (totalLen-1) ints (acc++".") + 
                  combs chs (ones - 1) (totalLen - 1) ints (acc++"#")
      (ch:chs) -> combs chs ones totalLen ints (acc++[ch])
      []       -> if stillValid acc ints (length acc + length s) then 1 else 0

isValid :: String -> [Int] -> Bool
isValid row ints = (L.map length . wordsBy (=='.') . L.filter (/='?') $ row) == ints

solveA :: [String] -> String
solveA = show . sum . L.map calcCombs . parseInput

solveB :: [String] -> String
solveB = show . sum . L.map (calcCombs . expand) . parseInput
  where
    expand (row, ints) = let row' = tail $ concat (replicate 5 ('?':row)); ints' = concat $ replicate 5 ints in (row', ints')

parseInput :: [String] -> [SpringRow]
parseInput = L.map (\l -> let [row, counts] = words l in (row, L.map read $ splitOn "," counts))