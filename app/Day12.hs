module Day12 (day12) where
import AocUtils
import Data.List.Split (splitOn, wordsBy)
import Data.Char
import Data.Map as M
import Data.Set as S (Set, empty, insert, singleton, union, map, foldr, fromList, filter, unions)
import Data.List as L
import Data.List.Utils (replace)
import Data.Function (fix)
import Data.Function.Memoize
{-# LANGUAGE BangPatterns #-}

day12 :: AocDay
day12 = MkDay 12 solveA solveB

type SpringRow = (String, [Int])

calcCombs :: SpringRow -> Int
calcCombs (row, ints) = {-ptrace binsToCheck-} fst binsToCheck
  where
    setQMarks r b = L.foldr (\ch (row', b') -> if ch == '?' then (head b' : row', L.drop 1 b') else (ch:row', b')) ([], b) r
    hashesNeeded = sum ints - charCount '#' row
    qmarks = charCount '?' row
    intPattern = L.take (length ints `div` 5) ints
    binsToCheck = combs M.empty row hashesNeeded qmarks ints "" intPattern

charCount :: Eq a => a -> [a] -> Int
charCount c = length . L.filter (==c)

stillValid :: String -> [Int] -> Int -> Bool
stillValid str ints total = if length str == total
  then
    length zipped == length ints && length zipped == length parts && all (\(s, i) -> length s == i) zipped
  else
    length begin == 1 && all (\(s, i) -> length s == i) zipped ||
    length parts <= length ints && all (\(s, i) -> length s == i) begin && all (\(s, i) -> length s <= i) zipped
  where
    parts = wordsBy (=='.') str
    zipped = zip parts ints
    (begin, end) = L.splitAt (length zipped - 1) zipped

combs memoMap s ones totalLen ints acc intPat = case x `M.lookup` memoMap of
  Just answer -> (answer, memoMap)
  _ -> 
    -- if not $ stillValid acc ints (length s + length acc) then (0, M.insert x 0 memoMap) else
        case s of
        ('?':chs) -> (right + left, M.insert x (right + left) memoMap'')
            where
              (right, memoMap'') = combs memoMap' chs ones (totalLen-1) ints (acc++".") intPat
              (left, memoMap') = combs memoMap chs (ones - 1) (totalLen - 1) ints (acc++"#") intPat
        (ch:chs) -> (middle, M.insert x middle mm)
          where
            (middle, mm) = combs memoMap chs ones totalLen ints (acc++[ch]) intPat
        []       -> if isValid acc ints then (1, M.insert x 1 memoMap) else (0, M.insert x 0 memoMap)
  where
    x = (accWords, intsSatisfied)
    intsSatisfied = zipWith (\i a -> i == length a) (ints++[0]) accWords
    accWords = wordsBy (=='.') acc
    qmarksLeft = charCount '?' s
    hashCount = charCount '#' (acc++s)
    dotCount = charCount '.' (acc++s)

isValid :: String -> [Int] -> Bool
isValid row ints = (L.map length . wordsBy (=='.') . L.filter (/='?') $ row) == ints

solveA :: [String] -> String
solveA = show . L.map calcCombs . parseInput

solveB :: [String] -> String
solveB input = show . sum . L.map (calcCombs . expand) . parseInput $ input
  where
    expand (row, ints) = (row++'?':row++'?':row++'?':row++'?':row, concat $ replicate 5 ints)
    largeAnswer = L.map (calcCombs . expand) . parseInput $ input

parseInput :: [String] -> [(String, [Int])]
parseInput = L.map (\l -> let [row, counts] = words l in (row, L.map read $ splitOn "," counts))

------Anders

splitRow :: (String, [Int]) -> ([String], [Int])
splitRow (row, ints) = (parts, ints)
  where
    parts = wordsBy (=='.') row

makeCombs :: ([String], [Int]) -> Int
makeCombs (part:parts, int:ints) | length part > int = 0
                                 | otherwise = undefined