module AocUtils where

import Data.List as L
import Data.Map as M

data Point2D = P2 {getX :: Int, getY :: Int}
  deriving (Eq)

type Vector = Point2D

vecLength :: Vector -> Int
vecLength (P2 x y) = abs x + abs y

manhattan :: Point2D -> Point2D -> Int
manhattan (P2 x1 y1) (P2 x2 y2) = abs (x2 - x1) + abs (y2 - y1)

instance Ord Point2D where
  compare p1 p2 = compare (vecLength p1) (vecLength p2)

instance Show Point2D where
  show (P2 x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

gauss :: Int -> Int
gauss n = n * (n + 1) `div` 2

choose :: Int -> Int -> Int
choose n k
  | n < 1 || k < 1 = error "No negative integers in a choose function"
  | otherwise = factorial n `div` (factorial k * factorial (n - k))

factorial :: Int -> Int
factorial 0 = 1
factorial n = product [1 .. n]

type Solver = [String] -> String

data AocDay = MkDay Int Solver Solver