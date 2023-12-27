module Day23 (day23) where
import AocUtils
import Debug.Trace

import qualified Data.Map as M
import qualified Data.Heap as H
import qualified Data.Set as S
import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple.Utils (fst3)

day23 :: AocDay
day23 = MkDay 23 solveA solveB

solveA :: [String] -> String
solveA input = show . abs . fromJust . M.lookup end $ allDists
  where
    (maxX, maxY) = (length . head $ input, length input)
    start = fst . head . M.toList $ M.filterWithKey (\(_, y) ch -> ch == '.' && y == 0) grid
    end = fst . head . M.toList $ M.filterWithKey (\(_, y) ch -> ch == '.' && y == maxY-1) grid
    allDists = negaDijkstra grid start
    grid = getGrid input

solveB :: [String] -> String
solveB input = show maxPath
  where
    maxY = length input
    totalDots = length . concatMap (filter (=='.')) $ input
    start = fst . head . M.toList $ M.filterWithKey (\(_, y) ch -> ch == '.' && y == 0) grid
    end = fst . head . M.toList $ M.filterWithKey (\(_, y) ch -> ch == '.' && y == maxY-1) grid
    graph = makeGraph grid
    maxPath = dfs graph start end totalDots
    grid = getGrid input

getJunctions :: M.Map Point Char -> [Point]
getJunctions grid = map fst . filter (\(pos, c) -> let ns = neighs pos in c /= '#' && (length ns > 2 || length ns == 1)) . M.toList $ grid
  where
    neighs (x, y) = [(i+x, j+y) | (i, j) <- [(0, 1), (0, -1), (1, 0), (-1, 0)], '#' /= fromMaybe '#' (M.lookup (x+i, y+j) grid)]

makeGraph :: M.Map Point Char -> M.Map Point [(Point, Int)]
makeGraph grid = foldr (\j m -> M.insert j (findNeighs j) m) M.empty junctions
  where
    junctions = getJunctions grid
    findNeighs j = map (\n -> goUntilFound junctions n (S.fromList [n, j]) 1) (neighs j S.empty)
    neighs (x, y) s = [(i+x, j+y) | (i, j) <- [(0, 1), (0, -1), (1, 0), (-1, 0)], '#' /= fromMaybe '#' (M.lookup (x+i, y+j) grid), (x+i, y+j) `S.notMember` s]
    goUntilFound :: [Point] -> Point -> S.Set Point -> Int -> (Point, Int)
    goUntilFound juncs pos visited total | pos `elem` juncs = (pos, total)
                                         | otherwise = goUntilFound juncs (head $ neighs pos visited) (S.insert pos visited) (total+1)

dfs :: M.Map Point [(Point, Int)] -> Point -> Point -> Int -> Int
dfs graph start end totalDots = dfs' start 0 S.empty
  where
    dfs' :: Point -> Int -> S.Set Point -> Int
    dfs' pos@(x, y) dist visited
      | pos `S.member` visited = 0
      | pos == end = dist
      | otherwise =
        foldr (\(pos', dist') acc -> let i = dfs' pos' dist' visited' in max i acc) 0 neighs
      where
        visited' = S.insert pos visited
        neighs = [(pos', dist + dist') | (pos', dist') <- fromJust $ M.lookup pos graph]

type Node = (Int, Point, Char)

getGrid :: [String] -> M.Map Point Char
getGrid = fst . foldl (\(m, y) row -> (fst $ foldl (\(m', x) ch -> (M.insert (x, y) ch m', x+1)) (m, 0) row, y + 1)) (M.empty, 0)

getQueue :: Point -> H.MinHeap Node
getQueue start = H.fromList [(0, start, 'd')]

getDistMap :: Point -> M.Map Point Int
getDistMap start = M.fromList [(start, 0)]

trd :: (a, b, c) -> c
trd (_, _, x) = x

negaDijkstra :: M.Map Point Char -> Point -> M.Map Point Int
negaDijkstra grid start = negaDijkstra' (getQueue start) (getDistMap start) S.empty
  where
    negaDijkstra' :: H.MinHeap Node -> M.Map Point Int -> S.Set Point -> M.Map Point Int
    negaDijkstra' queue distMap visited | H.isEmpty queue = distMap
                                        | otherwise =
      case H.view queue of
        Just (cur@(dist, pos@(x, y), dir), queue') | pos `S.member` visited -> negaDijkstra' queue' distMap' visited
                                                   | otherwise -> negaDijkstra' queue'' distMap' visited
                                                    where
                                                      visited' = S.insert pos visited
                                                      distMap' = foldr (\(dist', pos, _) dm -> if pos `M.notMember` dm || (dist' < fromJust (M.lookup pos dm)) then M.insert pos dist' dm else dm) distMap neighs
                                                      queue'' = foldr H.insert queue' neighs
                                                      currentChar = fromJust $ M.lookup pos grid
                                                      neighs = [(dist - 1, (i+x, j+y), d) | ((i, j), d) <- getDirections currentChar, d /= backwards dir, '#' /= fromMaybe '#' (M.lookup (x+i, y+j) grid), S.notMember (x+i, y+j) visited]
        _ -> error "error when getting head of queue"

backwards :: Char -> Char
backwards 'r' = 'l'
backwards 'u' = 'd'
backwards 'l' = 'r'
backwards 'd' = 'u'

getDirections :: Char -> [(Point, Char)]
getDirections '.' = [((0, 1), 'd'), ((0, -1), 'u'), ((1, 0), 'r'), ((-1, 0), 'l')]
getDirections '>' = [((1, 0), 'r')]
getDirections '<' = [((-1, 0), 'l')]
getDirections '^' = [((0, -1), 'u')]
getDirections 'v' = [((0, 1), 'd')]