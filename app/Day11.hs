module Day11 (day11) where
import AocUtils
import Data.Map as M (empty, insert, Map, fromList, lookup, singleton, toList, member, map, foldr)
import Data.List as L ( transpose, foldl, foldr, map, filter )
import Data.Set as S (empty, insert, Set, fromList, singleton, toList, member, union)
import Data.Maybe
import Data.Heap as H

day11 :: AocDay
day11 = MkDay 11 solveA solveB

data Space = Galaxy Int | EmptySpace
  deriving (Eq, Ord)

instance Show Space where
  show (Galaxy i) = "Galaxy: " ++ show i
  show _ = "Empty Space"

solveA :: [String] -> String
solveA input = show $ allDistances galaxies largeSpaces 2
  where
    largeSpaces = getLargeSpaces input
    galaxies = parseInput input

allDistances :: Map k Point -> Set Point -> Int -> Int
allDistances galaxies largeSpaces n = (`div` 2) . sum . concat $ M.foldr (\pos acc -> M.foldr (\pos2 acc' -> (n-1) * emptySize pos pos2 + manhattan pos pos2 : acc') [] galaxies : acc) [] galaxies
  where
    emptySize (x, y) (i, j) = L.foldr (\pos acc -> if pos `S.member` largeSpaces then acc+1 else acc) 0 ([(a, y) | a <- [min i x..max i x]] ++ [(x, b) | b <- [min j y..max y j]])

solveB :: [String] -> String
solveB input = show $ allDistances galaxies largeSpaces (10^6)
  where
    largeSpaces = getLargeSpaces input
    galaxies = parseInput input

getLargeSpaces :: [String] -> Set Point
getLargeSpaces lines = S.union (getEmptyRows True lines) (getEmptyRows False . transpose $ lines)
  where
      getEmptyRows b = fst . foldl (\(s, y) row -> if all (=='.') row then (fst . foldl (\(s', x) ch -> let pos = (if b then (x, y) else (y, x)) in (S.insert pos s', x+1)) (s, 0) $ row, y+1) else (s, y+1)) (S.empty, 0)

parseInput :: [String] -> Map Space Point
parseInput input = fst flattenList
  where
    flattenList = L.foldl (\(m, y) row -> (fst $ L.foldl (\(m', x) ch -> if ch == '.' then (m', x+1) else (M.insert (Galaxy (length m')) (x, y) m', x+1)) (m, 0) row, y+1)) (M.empty, 0) input