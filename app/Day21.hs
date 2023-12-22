module Day21 (day21) where
import AocUtils

import qualified Data.Map as M
import qualified Data.Heap as H
import qualified Data.Set as S
import Data.Maybe (fromMaybe, fromJust)

day21 :: AocDay
day21 = MkDay 21 solveA solveB


-- 621652828112028
-- 616951585159977
-- 616951585159978
-- 616951804315987
-- 613028455194097.8
-- 617557747193109.8

solveA :: [String] -> String
solveA input = show endPoints
  where
    (maxX, maxY) = (length . head $ input, length input)
    endPoints = [dijkstra grid (maxX, maxY) start steps | steps <- [65, 65+maxY, 65+2*maxY, 65+3*maxY]]
    start = fst . head . M.toList . M.filter (=='S') $ grid
    grid = getGrid input

solveB :: [String] -> String
solveB input = ""

type Node = (Int, Point)

getGrid :: [String] -> M.Map Point Char
getGrid = fst . foldl (\(m, y) row -> (fst $ foldl (\(m', x) ch -> (M.insert (x, y) ch m', x+1)) (m, 0) row, y + 1)) (M.empty, 0)

getQueue :: Point -> H.MinHeap Node
getQueue start = H.fromList [(0, start)]

getDistMap :: Point -> M.Map Point Int
getDistMap start = M.fromList [(start, 0)]

trd :: (a, b, c) -> c
trd (_, _, x) = x

dijkstra ::  M.Map Point Char -> Point -> Point -> Int -> Int
dijkstra grid (maxX, maxY) start end = dijkstra' (getQueue start) (getDistMap start) S.empty 0
  where
    dijkstra' :: H.MinHeap Node -> M.Map Point Int -> S.Set Point -> Int -> Int
    dijkstra' queue distMap visited total | H.isEmpty queue = total
                                          | otherwise =
      case H.view queue of
        Just (cur@(dist, pos@(x, y)), queue') | M.size distMap == -1 || pos `S.member` visited -> dijkstra' queue' distMap visited' total
                                              | otherwise -> dijkstra' queue'' distMap visited' (if even dist == even end then total + 1 else total)
                                                    where
                                                      visited' = S.insert pos visited
                                                      --distMap' = foldr (\(dist', pos) dm -> if pos `M.notMember` dm || (dist' < fromJust (M.lookup pos dm)) then M.insert pos dist' dm else dm) distMap neighs
                                                      queue'' = foldr H.insert queue' neighs
                                                      neighs = [(dist + 1, (i+x, j+y)) | (i, j) <- [(0, 1), (0, -1), (1, 0), (-1, 0)], '#' /= fromMaybe '#' (M.lookup ((x+i) `mod` maxX, (y+j) `mod` maxY) grid), dist+1 <= end, S.notMember (x+i, y+j) visited]
        _ -> error "error when getting head of queue"