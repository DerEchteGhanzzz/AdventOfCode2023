module Day17 (day17) where
import AocUtils

import qualified Data.Map as M
import qualified Data.Heap as H
import qualified Data.Set as S
import Data.Tuple.Utils (fst3, snd3)
import Data.Maybe (fromJust)

day17 :: AocDay
day17 = MkDay 17 solveA solveB

solveA :: [String] -> String
solveA input = show . dijkstra (0, 3) grid start $ end
  where
    start = (0, 0)
    end = ((length . head $ input) - 1, length input - 1)
    grid = getGrid input

solveB :: [String] -> String
solveB input = show . dijkstra (4, 10) grid start $ end
  where
    start = (0, 0)
    end = ((length . head $ input) - 1, length input - 1)
    grid = getGrid input

getGrid :: [String] -> M.Map Point Int
getGrid = fst . foldl (\(m, y) row -> (fst $ foldl (\(m', x) ch -> (M.insert (x, y) (read [ch]) m', x+1)) (m, 0) row, y + 1)) (M.empty, 0)

backwards :: Char -> Char
backwards '>' = '<'
backwards '^' = 'v'
backwards '<' = '>'
backwards 'v' = '^'

type Node = (Int, Point, (Char, Int))

getQueue :: Point -> H.MinHeap Node
getQueue start = H.fromList [(0, start, ('>', 0)), (0, start, ('v', 0))]

getDistMap :: Point -> M.Map (Point, (Char, Int)) Int
getDistMap start = M.fromList [((start, ('>', 0)), 0), ((start, ('v', 0)), 0)]
trd :: (a, b, c) -> c
trd (_, _, x) = x

dijkstra :: (Int, Int) -> M.Map Point Int -> Point -> Point -> Int
dijkstra (minLen, maxLen) grid start end = dijkstra' (getQueue start) (getDistMap start) S.empty
  where
    dijkstra' :: H.MinHeap Node -> M.Map (Point, (Char, Int)) Int -> S.Set (Point, Char, Int) -> Int
    dijkstra' queue distMap visited | H.isEmpty queue = -1
                                    | otherwise =
      case H.view queue of
        Just (cur@(dist, pos@(x, y), (dir, len)), queue') | pos == end -> dist
                                                          | (pos, dir, len) `S.member` visited -> dijkstra' queue' distMap' visited
                                                          | otherwise -> dijkstra' queue'' distMap' visited'
                                                    where
                                                      visited' = S.insert (pos, dir, len) visited
                                                      distMap' = foldr (\(dist', pos, d) dm -> if (pos, d) `M.member` dm && (dist' < fromJust (M.lookup (pos, d) dm)) then M.insert (pos, d) dist' dm else dm) distMap neighs
                                                      newLen d = if d == dir then len + 1 else 1
                                                      queue'' = foldr H.insert queue' neighs
                                                      neighs = [(dist + fromJust (M.lookup (i+x, j+y) grid), (i+x, j+y), (d, newLen d)) | (i, j, d) <- [(0, 1, '^'), (0, -1, 'v'), (1, 0, '>'), (-1, 0, '<')], d /= backwards dir, (i + x, j + y) `M.member` grid, len < maxLen || d /= dir, len > minLen || d == dir]
        _ -> error "error when getting head of queue"
