module Day18 (day18) where
import AocUtils

import Data.Map as M
import Data.List as L
import Data.Set as S
import Data.Char
import qualified Data.Bifunctor

day18 :: AocDay
day18 = MkDay 18 solveA solveB

hexToDecimal :: String -> Integer
hexToDecimal = L.foldl (\acc x -> acc * 16 + hexDigitToInt x) 0
   where
    hexDigitToInt x
          | isDigit x = toInteger $ ord x - ord '0'
          | x >= 'a' && x <= 'f' = toInteger $ ord x - ord 'a' + 10
          | otherwise = error $ show x

digTrench :: Point -> [Point] -> [String] -> [Point]
digTrench _ m [] = m
digTrench pos@(x,y) m (instr:is) = digTrench pos' m' is
  where
    [dir, amtStr, color] = words instr
    amt = read amtStr
    m' = m ++ [pos']
    pos' = case dir of
      "R" -> (x+amt, y)
      "L" -> (x-amt, y)
      "U" -> (x, y+amt)
      "D" -> (x, y-amt)
      _ -> error $ "unknown pattern: " ++ show dir

type Line = (Point, Point)

digLargeTrench :: IPoint -> [IPoint] -> [String] -> [IPoint]
digLargeTrench _ s [] = s
digLargeTrench pos@(x,y) s (instr:is) = digLargeTrench pos' s' is
  where
    [_, _, color] = words instr
    amt = hexToDecimal (L.take 5 $ L.drop 2 color) --read amtStr
    newDir = color !! 7
    s' = s ++ [pos]
    pos' = case newDir of
      '0' -> (x+amt, y)
      '2' -> (x-amt, y)
      '3' -> (x, y-amt)
      '1' -> (x, y+amt)
      _ -> error $ "unknown pattern: " ++ show newDir

solveA :: [String] -> String
solveA input = show . sum $ [shoelace (minX, minY) trench, circomverence `div` 2, 1]
  where
    trench = L.map (Data.Bifunctor.bimap toInteger toInteger) $ digTrench (0,0) [] input
    minX = toInteger . minimum . L.map fst $ trench
    minY = toInteger . minimum . L.map snd $ trench
    circomverence = fst $ L.foldr (\p (acc, prevP) -> (acc + iManhattan p prevP, p)) (0, (0, 0)) trench

solveB :: [String] -> String
solveB input = show . sum $ [shoelace (minX, minY) trench, circomverence `div` 2, 1]
  where
    trench = digLargeTrench (0,0) [] input
    minX = toInteger . minimum . L.map fst $ trench
    minY = toInteger . minimum . L.map snd $ trench
    circomverence = fst $ L.foldr (\p (acc, prevP) -> (acc + iManhattan p prevP, p)) (0, (0, 0)) trench

iManhattan (x, y) (a, b) = abs (x - a) + abs (y - b)

type IPoint = (Integer, Integer)

shoelace :: Integral a => (a, a) -> [(a, a)] -> a
shoelace (minX, minY) s = abs $ area `div` 2
  where
    area = L.foldl (\tot ((x1, y1), (x2, y2)) -> (x1-minX)*(y2-minY) - (y1-minY)*(x2-minX) + tot) 0 zipped
    zipped = zip s (tail s ++ [head s])
    s' = L.take (length s - 1) s