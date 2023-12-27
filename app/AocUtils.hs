module AocUtils where

import Data.List as L
import Data.Map as M
import Debug.Trace

ptrace :: Show a => a -> b -> b
ptrace a b | trace (show a) False = undefined
           | otherwise = b

type Point = (Int, Int)

type Vector = Point

vecLength :: Vector -> Int
vecLength (x, y) = abs x + abs y

manhattan :: Num a => (a, a) -> (a, a) -> a
manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

gauss :: Int -> Int
gauss n = n * (n + 1) `div` 2

choose :: Int -> Int -> Int
choose n k
  | n < 1 || k < 1 = error "No negative integers in a choose function"
  | otherwise = factorial n `div` (factorial k * factorial (n - k))

factorial :: Int -> Int
factorial 0 = 1
factorial n = product [1 .. n]

getElem2D :: [[a]] -> Point -> Maybe a
getElem2D array2d (x, y) = do
      row <- if y < length array2d && y >= 0 then Just (array2d !! y) else Nothing
      if x < length row && x >= 0 then Just (row !! x) else Nothing

type Solver = [String] -> String

data AocDay = MkDay Int Solver Solver