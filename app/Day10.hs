module Day10 (day10) where
import AocUtils

import Graphics.Gloss hiding (Point)
import Data.Map as M (fromList, lookup, insert, Map, foldr, toList, empty, member)
import Data.Set as S (fromList, insert, Set, foldr, toList, union, empty, member)
import Data.List as L
import Data.Maybe
import Debug.Trace

data Pipe = NW | NE | NS | EW | SE | SW | Ground | Start
  deriving (Show, Eq, Ord)

chToPipe :: Char -> Pipe
chToPipe '|' = NS
chToPipe '-' = EW
chToPipe 'L' = NE
chToPipe 'J' = NW
chToPipe '7' = SW
chToPipe 'F' = SE
chToPipe '.' = Ground
chToPipe 'S' = Start
chToPipe ch  = error $ show ch ++ " is not a valid pipe"

searchStart :: Map FPoint Pipe -> FPoint
searchStart pipeMap = fst $ L.foldr (\(pos, pipe) (target, found) -> if pipe == Start || found then (pos, found) else (target, found)) ((0,0), False) (M.toList pipeMap)


searchDirection :: FPoint -> Map FPoint Pipe -> [(FPoint, Pipe)]
searchDirection (x, y) pipeMap = mapMaybe filterPos [(i, j) |
                                    i <- [x - 1 .. x + 1],
                                    j <- [y - 1 .. y + 1],
                                    i == x && j /= y || i /= x && j == y]
  where
    getPipe pos = pos `M.lookup` pipeMap
    filterPos (i, j) | x - i == 1 && getPipe (i, j) `elem` [Just NE, Just SE, Just EW]    = Just ((i, j), fromJust $ getPipe (i, j))
                     | x - i == (-1) && getPipe (i, j) `elem` [Just NW, Just SW, Just EW] = Just ((i, j), fromJust $ getPipe (i, j))
                     | y - j == 1 && getPipe (i, j) `elem` [Just SW, Just SE, Just NS]    = Just ((i, j), fromJust $ getPipe (i, j))
                     | y - j == (-1) && getPipe (i, j) `elem` [Just NW, Just NE, Just NS] = Just ((i, j), fromJust $ getPipe (i, j))
                     | otherwise = Nothing

day10 :: AocDay
day10 = MkDay 10 solveA solveB

type FPoint = (Float, Float)

traversePipe :: FPoint -> FPoint -> (FPoint, Pipe) -> Int -> Map FPoint Pipe -> Map FPoint (Int, Pipe) -> Map FPoint (Int, Pipe)
traversePipe start (pX, pY) ((x, y), pipe) dist pipeMap distMap = if (x, y) == start then distMap else
  traversePipe start (x, y) ((x', y'), pipe') dist' pipeMap distMap''
  where
    (x', y') = case pipe of
      NS -> if y - pY == 1 then (x, y+1) else (x, y-1)
      EW -> if x - pX == 1 then (x+1, y) else (x-1, y)
      NW -> if y - pY == 1 then (x-1, y) else (x, y-1)
      NE -> if y - pY == 1 then (x+1, y) else (x, y-1)
      SW -> if y - pY == (-1) then (x-1, y) else (x, y+1)
      SE -> if y - pY == (-1) then (x+1, y) else (x, y+1)
      _  -> error $ show (pipe, (x', y')) ++ " not in path"
    pipe' = fromJust $ (x', y') `M.lookup` pipeMap
    dist' = dist + 1
    distMap'' = M.insert ( (pX - x) / 2.0 + x, (pY - y) / 2.0 + y) (0, pipe) distMap'
    distMap' = case (x, y) `M.lookup` distMap of
      Nothing -> M.insert (x, y) (dist, pipe) distMap
      Just (d, p) -> if dist < d then M.insert (x, y) (dist, p) distMap else distMap

getPipeMap :: Map FPoint Pipe -> Map FPoint (Int, Pipe)
getPipeMap pipeMap = traversePipe start start (last starts) 1 pipeMap distMap
  where
    distMap = traversePipe start start (head starts) 1 pipeMap (M.fromList [((sx, sy), (0, Start))])
    start@(sx, sy) = searchStart pipeMap
    starts = searchDirection start pipeMap

solveA :: [String] -> String
solveA input = show $ M.foldr (max . fst) 0 distMap
  where
    distMap = getPipeMap pipeMap
    pipeMap = parseInput input

solveB :: [String] -> String
solveB input = show $ length innerSpace
  where
    loopMap = L.foldr (\(p, _) s -> p `S.insert` s) S.empty (M.toList $ getPipeMap pipeMap)
    pipeMap = parseInput input
    innerSpace = filter (\((x, y), _) -> not (S.member (x, y) emptySpace)) (M.toList pipeMap)
    emptySpace = loopMap `S.union` floodFill loopMap pipeMap (fromIntegral $ length (head input), fromIntegral $ length input)

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

floodFill :: Set FPoint -> Map FPoint Pipe -> FPoint -> Set FPoint
floodFill loopSet pipeSet (maxX, maxY) = floodFill' [(0, 0)] S.empty
  where
    floodFill' :: [FPoint] -> Set FPoint -> Set FPoint
    floodFill' [] visited = visited
    floodFill' (pos@(x, y):queue) visited = if pos `S.member` loopSet || x < 0 || y < 0 || x >= maxX || y >= maxY || pos `S.member` visited then floodFill' queue visited' else
      floodFill' (queue ++ [(x+i, y+j) | i <- [-0.5, 0, 0.5], j <- [-0.5, 0, 0.5], i /= j]) visited'
      where
        visited' = pos `S.insert` visited

parseInput :: [String] -> Map FPoint Pipe
parseInput input = fst $ L.foldl (\(m, y) row -> (fst $ L.foldl (\(m', x) ch -> (M.insert (x, y) (chToPipe ch) m', x+1)) (m, 0) row, y+1)) (M.empty, 0) input