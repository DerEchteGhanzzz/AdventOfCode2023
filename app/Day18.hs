module Day18 (day18) where
import AocUtils hiding (Point)

import Data.Map as M
import Data.List as L
import Data.Set as S
import Data.Char
import qualified Data.Bifunctor

day18 :: AocDay
day18 = MkDay 18 solveA solveB

type Point = (Integer, Integer)

data Direction = R | D | L | U
  deriving (Show, Eq, Ord, Enum, Read)

hexToDecimal :: String -> Integer
hexToDecimal = L.foldl (\acc x -> acc * 16 + hexDigitToInt x) 0 . L.map toUpper
   where
    hexDigitToInt x
          | isDigit x = toInteger $ ord x - ord '0'
          | x >= 'A' && x <= 'F' = toInteger $ ord x - ord 'A' + 10
          | otherwise = error $ show x

digTrench :: Point -> [Point] -> [(Direction, Integer)] -> [Point]
digTrench _ m [] = m
digTrench pos@(x,y) m ((dir, amt):is) = digTrench pos' m' is
  where
    m' = m ++ [pos']
    pos' = case dir of
      R -> (x+amt, y)
      L -> (x-amt, y)
      U -> (x, y-amt)
      D -> (x, y+amt)

shoelace :: Integral a => (a, a) -> [(a, a)] -> a
shoelace (minX, minY) s = abs $ area `div` 2
  where
    area = L.foldl (\tot ((x1, y1), (x2, y2)) -> (x1-minX)*(y2-minY) - (y1-minY)*(x2-minX) + tot) 0 zipped
    zipped = zip s (tail s ++ [head s])

getArea :: [(Direction, Integer)] -> Integer
getArea instrs = shoelace (minX, minY) trench + circomverence `div` 2 + 1
  where
    trench = digTrench (0, 0) [] instrs
    minX = toInteger . minimum . L.map fst $ trench
    minY = toInteger . minimum . L.map snd $ trench
    circomverence = fst $ L.foldr (\p (acc, prevP) -> (acc + manhattan p prevP, p)) (0, head trench) trench

solveA :: [String] -> String
solveA = show . getArea . L.map (\str -> let [dir, amt, _] = words str in (read dir, read amt))

solveB :: [String] -> String
solveB = show . getArea . parseInput
  where
    parseInput = L.map parseLine
    parseLine str = let hexCode = L.filter isAlphaNum . last . words $ str in (toDir . last $ hexCode, hexToDecimal . init $ hexCode)
    toDir i = toEnum . read $ [i]