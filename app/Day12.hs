module Day12 (day12) where
import AocUtils
import Data.List.Split (splitOn, wordsBy, splitWhen)
import Data.Char
import Data.Map as M
import Data.Set as S (Set, empty, insert, singleton, union, map, foldr, fromList, filter, unions)
import Data.List as L
import Data.List.Utils (replace)
import Data.Function (fix)
import Data.Function.Memoize

day12 :: AocDay
day12 = MkDay 12 solveA solveB

type SpringRow = (String, [Int])

calcCombs :: SpringRow -> Int
calcCombs (row, ints) = {-ptrace binsToCheck-} fst binsToCheck
  where
    setQMarks r b = L.foldr (\ch (row', b') -> if ch == '?' then (head b' : row', L.drop 1 b') else (ch:row', b')) ([], b) r
    hashesNeeded = sum ints - charCount '#' row
    qmarks = charCount '?' row
    binsToCheck = combs M.empty "" row ints 0 0

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

combs :: Map (Int, Int) Int -> String -> String -> [Int] -> Int -> Int -> (Int, Map (Int, Int) Int)
combs memoMap seenChars unseenChars ints index intIndex = case x `M.lookup` memoMap of
  Just answer -> ptrace ("cachehit", x, seenChars, unseenChars, answer) (answer, memoMap)
  _ ->
        case unseenChars of
        ('?':chs) -> (right + left, M.insert x (right + left) memoMap'')
            where
              (right, memoMap') = if goSubproblem "." then 
                let (ans, m) = combs memoMap "" chs (L.drop 1 ints) (index+1) (intIndex+1) in (ans, M.insert x ans m)
              else combs memoMap  (seenChars++".") chs ints (index+1) intIndex
              (left, memoMap'') = if goSubproblem "#" then 
                let (ans, m) = combs memoMap "" chs (L.drop 1 ints) (index+1) (intIndex+1) in (ans, M.insert x ans m)
              else combs memoMap  (seenChars++"#") chs ints (index+1) intIndex
        (ch:chs) -> (middle, M.insert x middle mm)
          where
            (middle, mm) = combs memoMap (seenChars ++ [ch]) chs ints (index+1) intIndex
        []       -> ptrace ("end", x, seenChars++unseenChars) $ if isValid seenChars ints then ptrace 1 (1, M.insert x 1 memoMap) else ptrace 0 (0, M.insert x 0 memoMap)
  where
    x = (index, intIndex)
    goSubproblem c = (seenChars ++ c) `satisfies` ints
satisfies :: String -> [Int] -> Bool
satisfies []  []   = True
satisfies row []   = False
satisfies []  ints = False
satisfies row ints = '#' `elem` row && length (head hashWords) == head ints && last row == '.' 
  where
    hashWords = wordsBy (=='.') row

isValid :: String -> [Int] -> Bool
isValid row ints = (L.map length . wordsBy (=='.') . L.filter (/='?') $ row) == ints

solveA :: [String] -> String
solveA = show . L.map calcCombs . parseInput

solveB :: [String] -> String
<<<<<<< HEAD
solveB input = "nee"
=======
solveB input = ""--show . sum . L.map (calcCombs . expand) . parseInput $ input
>>>>>>> c4bca38caa88498883660e4d8af429107b081735
  where
    expand (row, ints) = (row++'?':row++'?':row++'?':row++'?':row, concat $ replicate 5 ints)
    largeAnswer = L.map (calcCombs . expand) . parseInput $ input

parseInput :: [String] -> [(String, [Int])]
parseInput = L.map (\l -> let [row, counts] = words l in (row, L.map read $ splitOn "," counts))