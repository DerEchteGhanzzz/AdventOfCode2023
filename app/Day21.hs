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
solveB input = show (orange + darkGreenMidline + lightGreenMidline + darkGreenPiramids + lightGreenPiramids)
  where
    totalSteps = 26501365
    correction = radius * 96
    radius = 202300
    orange = M.size oddFullSquare + M.size oddMiddle
    darkGreenPiramids = 2 * gauss radius * M.size oddFullSquare
    lightGreenPiramids = 2 * gauss (radius - 1) * M.size evenFullSquare
    lightGreenMidline = 202300 * M.size evenFullSquare
    darkGreenMidline = (202300 + 1 - 2) * M.size oddFullSquare
    
    (maxX, maxY) = (length . head $ input, length input)
    endings = dijkstra grid (maxX, maxY) start 131
    oddMiddle = M.filterWithKey (\pos _ -> manhattan start pos <= 65) oddFullSquare
    evenMiddle = M.filterWithKey (\pos _ -> manhattan start pos <= 65) evenFullSquare
    -- corners@[oddTopLeft, oddTopRight, oddBotLeft, oddBotRight] = map (\(a, b) -> M.filterWithKey (\(x, y) _ -> a x && b y && (x, y) `M.notMember` oddMiddle) oddFullSquare) [((<sX), (>sY)), ((sX<), (>sY)), ((<sX), (<sY)), ((sX<), (<sY))]
    (oddFullSquare, evenFullSquare) = M.foldrWithKey (\(x, y) dist (od, ev) -> if x < maxX && x >= 0 && y < maxY && y >= 0 then
                                                                if even dist then (od, M.insert (x, y) dist ev) else (M.insert (x, y) dist od, ev)
                                                                else (od, ev)) (M.empty, M.empty) endings
    start@(sX, sY) = fst . head . M.toList . M.filter (=='S') $ grid
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