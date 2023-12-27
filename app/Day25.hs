module Day25 (day25) where
import AocUtils

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Heap as H
import Data.Maybe (fromJust)
import Data.List (sortBy)
import Data.Ord

day25 :: AocDay
day25 = MkDay 25 solveA solveB

solveA :: [String] -> String
solveA input = show graph
  where
    connections = foldl (\acc (key, vals) -> acc ++ [S.foldl (\acc x -> (key, x) : acc) [] vals]) [] sortedGraph
    sortedGraph = sortBy (\(_, a) (_, b) -> Down (S.size a) `compare` Down (S.size b)) . M.toList $ graph
    (x, y) = cutOne graph (M.size graph) allConsList
    allConsList = S.foldr (\s acc -> let [c1, c2] = S.toList s in (c1, c2) : acc) [] allCons
    allCons = getAllConnections graph
    graph = getMap input
    start = fst . head . M.toList $ graph

cutOne :: M.Map String (S.Set String) -> Int -> [(String, String)] -> (Int, Int)
cutOne graph s ((start, end):cuts) | s' <= 2 || s - s' <= 2 = cutOne graph s cuts
                                   | s' /= s = (s', s - s')
                                   | otherwise = cutOne graph' s cuts
  where
    startDegree = S.size . fromJust . M.lookup start $ graph
    endDegree = S.size . fromJust . M.lookup end $ graph
    s' = connectedSize graph' start
    graph' =
      let
          g = M.adjust (end `S.delete`) start graph
          g' = M.adjust (start `S.delete`) end g
      in if startDegree > 1 && endDegree > 1 then g' else graph

solveB :: [String] -> String
solveB _ = "Merry Christmas!"

getOddOnes :: M.Map String (S.Set String) -> S.Set String
getOddOnes m = M.foldrWithKey (\key1 vals1 s -> S.foldr (\key2 s' -> let vals2 = fromJust $ M.lookup key2 m in if S.size (S.intersection vals1 vals2) == 1 then S.insert (key1 ++ "-" ++ key2) s' else s') s vals1) S.empty m

getAllConnections :: M.Map String (S.Set String) -> S.Set (S.Set String)
getAllConnections = M.foldrWithKey (\key1 vals1 s -> S.foldr (\key2 s' -> S.insert (S.fromList [key1, key2]) s') s vals1) S.empty

getMap :: [String] -> M.Map String (S.Set String)
getMap = foldr (\line m -> let (key:vals) = words . filter (/=':') $ line; m' = M.insertWith S.union key (S.fromList vals) m in
  foldr (\v m'' -> M.insertWith S.union v (S.singleton key) m'') m' vals) M.empty

getQueue :: String -> H.MinHeap (Int, String)
getQueue start = H.fromList [(0, start)]

getDistMap :: String -> M.Map String Int
getDistMap start = M.fromList [(start, 0)]

connectedSize :: M.Map String (S.Set String) -> String ->  Int
connectedSize graph start = connectedSize' (getQueue start) S.empty
  where
    connectedSize' :: H.MinHeap (Int, String) -> S.Set String -> Int
    connectedSize' queue visited | H.isEmpty queue = S.size visited
                                 | otherwise =
      case H.view queue of
        Just (cur@(dist, pos), queue') | pos `S.member` visited -> connectedSize' queue' visited
                                       | otherwise -> connectedSize' queue'' visited'
                                                    where
                                                      visited' = S.insert pos visited
                                                      queue'' = foldr H.insert queue' neighs
                                                      neighs = [(dist + 1, neigh) |
                                                                  neigh <- S.toList . fromJust . M.lookup pos $ graph,
                                                                  S.notMember neigh visited]
        _ -> error "error when getting head of queue"