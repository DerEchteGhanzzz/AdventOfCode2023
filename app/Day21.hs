module Day21 (day21) where
import AocUtils

import qualified Data.Map as M
import qualified Data.Heap as H
import qualified Data.Set as S
import Data.Maybe (fromMaybe, fromJust)

day21 :: AocDay
day21 = MkDay 21 solveA solveB


-- 616951804315987

solveA :: [String] -> String
solveA input = show $ M.size endPoints
  where
    (maxX, maxY) = (length . head $ input, length input)
    endPoints = M.filter even $ dijkstra grid (maxX, maxY) start 64
    start = fst . head . M.toList . M.filter (=='S') $ grid
    grid = getGrid input

solveB :: [String] -> String
solveB input = show (M.size evenEndings, M.size oddEndings, M.size evenMiddle, M.size oddMiddle)
  where
    (maxX, maxY) = (length . head $ input, length input)
    endings = dijkstra grid (maxX, maxY) start 131
    oddMiddle = M.filter (<=65) oddEndings
    evenMiddle = M.filter (<=65) evenEndings
    -- (evenTopLeft, evenTopRight, evenBotLeft, evenBotRight) = map (\(a, b) -> M.filterWithKey (\(x, y) -> a x && b y) evenEndings) [(), (), (), (), ()]
    evenEndings = M.filterWithKey (\(x, y) dist -> x < maxX && x >= 0 && y < maxY && y >= 0 && even dist) endings
    oddEndings = M.filterWithKey (\(x, y) dist -> x < maxX && x >= 0 && y < maxY && y >= 0 && odd dist) endings
    start = fst . head . M.toList . M.filter (=='S') $ grid
    grid = getGrid input

type Node = (Int, Point)

getGrid :: [String] -> M.Map Point Char
getGrid = fst . foldl (\(m, y) row -> (fst $ foldl (\(m', x) ch -> (M.insert (x, y) ch m', x+1)) (m, 0) row, y + 1)) (M.empty, 0)

getQueue :: Point -> H.MinHeap Node
getQueue start = H.fromList [(0, start)]

getDistMap :: Point -> M.Map Point Int
getDistMap start = M.fromList [(start, 0)]

trd :: (a, b, c) -> c
trd (_, _, x) = x

dijkstra ::  M.Map Point Char -> Point -> Point -> Int -> M.Map Point Int
dijkstra grid (maxX, maxY) start end = dijkstra' (getQueue start) (getDistMap start) S.empty
  where
    dijkstra' :: H.MinHeap Node -> M.Map Point Int -> S.Set Point -> M.Map Point Int
    dijkstra' queue distMap visited | H.isEmpty queue = distMap
                                    | otherwise =
      case H.view queue of
        Just (cur@(dist, pos@(x, y)), queue') | M.size distMap == -1 || pos `S.member` visited -> dijkstra' queue' distMap' visited'
                                              | otherwise -> dijkstra' queue'' distMap' visited'
                                                    where
                                                      visited' = S.insert pos visited
                                                      distMap' = foldr (\(dist', pos) dm -> if pos `M.notMember` dm || (dist' < fromJust (M.lookup pos dm)) then M.insert pos dist' dm else dm) distMap neighs
                                                      queue'' = foldr H.insert queue' neighs
                                                      neighs = [(dist + 1, (i+x, j+y)) | (i, j) <- [(0, 1), (0, -1), (1, 0), (-1, 0)], '#' /= fromMaybe '#' (M.lookup ((x+i) `mod` maxX, (y+j) `mod` maxY) grid), dist+1 <= end, S.notMember (x+i, y+j) visited]
        _ -> error "error when getting head of queue"