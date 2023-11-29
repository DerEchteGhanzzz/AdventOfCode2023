module Dijkstra where

import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Set as S

newtype PrioQueue a b = Q [(a, b)]

qCutLine :: a -> b -> PrioQueue a b -> PrioQueue a b
qCutLine a b (Q as) = Q ((a, b) : as)

qView :: PrioQueue a b -> Maybe ((a, b), PrioQueue a b)
qView (Q []) = Nothing
qView (Q (a : as)) = Just (a, Q as)

qPop :: PrioQueue a b -> ((a, b), PrioQueue a b)
qPop (Q (a : as)) = (a, Q as)
qPop (Q []) = error "Empty queue"

qInsert :: (Ord a) => a -> b -> PrioQueue a b -> PrioQueue a b
qInsert x v (Q as) = Q (ordInsert x v as)
  where
    ordInsert v x [] = [(v, x)]
    ordInsert xv x q@((yv, y) : ys)
      | xv <= yv = (xv, x) : q
      | otherwise = (yv, y) : ordInsert xv x ys

data Distance a = Dist a | Infinity
  deriving (Show, Eq)

instance (Ord a) => Ord (Distance a) where
  Infinity <= Infinity = True
  Infinity <= Dist x = False
  Dist x <= Infinity = True
  Dist x <= Dist y = x <= y

instance (Num a) => Monoid (Distance a) where
  mempty = Dist 0

instance (Num a) => Semigroup (Distance a) where
  Dist x <> Dist y = Dist (x + y)
  _ <> _ = Infinity

type Graph a = Map a [(a, Distance Int)]

(!??) :: (Ord k) => Map k (Distance d) -> k -> Distance d
(!??) distanceMap key = fromMaybe Infinity (M.lookup key distanceMap)

data DijkstraState a = DijkstraState
  { visitedSet :: Set a,
    distanceMap :: Map a (Distance Int),
    queue :: PrioQueue (Distance Int) a
  }

dijkstra :: (Ord a) => Graph a -> a -> a -> Distance Int
dijkstra graph start end = undefined
  where
    visited = S.empty
    distanceMap = M.singleton start (Dist 0)
    queue = Q [(Dist 0, start)]
    state = DijkstraState visited distanceMap queue

    processQueue ds@(DijkstraState v d q) = case qView q of
      Nothing -> d
      Just ((minDist, vertex), q') ->
        if vertex == end
          then d
          else
            if S.member vertex v
              then processQueue (ds {queue = q'})
              else processQueue $ L.foldl (foldNeigh vertex) (DijkstraState v' d q') notVisitedNeighs
        where
          v' = S.insert vertex v
          allNeighs = fromMaybe [] $ M.lookup vertex graph
          notVisitedNeighs = L.filter (\(n, _) -> not (S.member n v')) allNeighs
          foldNeigh current ds@(DijkstraState _ d _) (neigh, weight) =
            let altDistance = (d !?? current) <> weight
             in if altDistance < d !?? neigh
                  then DijkstraState v' (M.insert neigh altDistance d) (qInsert altDistance neigh q')
                  else ds