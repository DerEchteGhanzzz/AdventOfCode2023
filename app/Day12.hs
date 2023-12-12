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
getCombinations = L.foldr ((:) . length . calcCombs) []

calcCombs :: SpringRow -> Set String
calcCombs (row, ints) = S.filter (`isValid` ints) binsToCheck
  where
    setQMarks r b = L.foldr (\ch (row', b') -> if ch == '?' then (head b' : row', drop 1 b') else (ch:row', b')) ([], b) r
    hashesNeeded = sum ints - charCount '#' row
    qmarks = charCount '?' row
    binsToCheck = combs row hashesNeeded qmarks
    charCount c = length . L.filter (==c)
    --S.foldr (\b acc -> let row' = fst $ setQMarks row b in if isValid row' ints then row' : acc else acc) [] binsToCheck

combs :: String -> Int -> Int -> Set String
combs s ones totalLen = case s of
  ('?':chs) -> case (ones, totalLen) of
    (0, _) -> S.singleton ('.' : replace "?" "." chs)
    (_, _) -> if ones == totalLen then S.singleton ('#' : replace "?" "#" chs)
                else S.map ('.':) (combs chs ones (totalLen-1)) `S.union` S.map ('#':) (combs chs (ones - 1) (totalLen - 1))
  (ch:chs) -> S.map (ch:) (combs chs ones totalLen)
  []       -> S.singleton []

isValid :: String -> [Int] -> Bool
isValid row ints = (L.map length . wordsBy (=='.') . L.filter (/='?') $ row) == ints

solveA :: [String] -> String
solveA = show . sum . L.map (length . calcCombs) . parseInput

intLogBase :: Int -> Int -> Int
intLogBase b n = round $ logBase (fromIntegral b) (fromIntegral n)

solveB :: [String] -> String
solveB input = show old
  where
    parsedInput = parseInput input
    new = L.map ((length . calcCombs) . (\(row, ints) -> let row' = '?':row; ints' = concat $ replicate 2 ints in ('?':row++['?'], ints))) parsedInput
    old = L.map (length . calcCombs) parsedInput

parseInput :: [String] -> [SpringRow]
parseInput = L.map (\l -> let [row, counts] = words l in (row, L.map read $ splitOn "," counts))
