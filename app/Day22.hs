-- {-# OPTIONS_GHC -Wall #-}

module Day22 (day22) where
import AocUtils
import Data.List.Split (splitOn)
import Data.Tuple.Utils (fst3, snd3)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Foldable (find)


day22 :: AocDay
day22 = MkDay 22 solveA solveB

type Point3 = (Int, Int, Int)
type Brick = (Point3, Point3, Int)

brickIntersect :: Brick -> Brick -> Bool
brickIntersect (a, b, _) (c, d, _) = overlap1D (fst3 a, fst3 b) (fst3 c, fst3 d) &&
                                     overlap1D (snd3 a, snd3 b) (snd3 c, snd3 d) &&
                                     overlap1D (trd a, trd b) (trd c, trd d)

overlap1D :: Point -> Point -> Bool
overlap1D (a, b) (c, d) = not (max a b < min c d || min a b > max c d)

trd :: (a, b, c) -> c
trd (_, _, c) = c

fall1 :: Brick -> Brick
fall1 ((a, b, c), (d, e, f), ide) = ((a-1, b, c), (d-1, e, f), ide)

turnOnGravity :: S.Set Brick -> [Brick] -> S.Set Brick
turnOnGravity onGround [] = onGround
turnOnGravity onGround (b:bs) = turnOnGravity onGround' bs
  where
    onGround' = S.insert (letFall b) onGround
    letFall :: Brick -> Brick
    letFall (b, e, ide) = if fst3 b' < 1 || S.foldr (\placed hasIntersect -> hasIntersect || brickIntersect brick' placed) False onGround
                       then (b, e, ide)
                       else letFall brick'
      where
        brick'@(b', e', ide') = fall1 (b, e, ide)

getDeleteSet :: S.Set Brick -> S.Set Brick
getDeleteSet onGround = S.fromList . map fst .
    filter (\(b, aboves) -> all (\a -> let supports = fromJust $ M.lookup a supportedByMap in S.size supports >= 2) aboves) $ supportsList
  where
    supportsList = M.toList supportsMap
    supportsMap = S.foldr (\b supported -> M.insert b (getAboveBricks onGround b) supported) M.empty onGround
    supportedByMap = S.foldr (\b supported -> M.insert b (getBelowBricks onGround b) supported) M.empty onGround

directlyAbove :: Brick -> Brick -> Bool
directlyAbove aboveBrick = brickIntersect (fall1 aboveBrick)

getAboveBricks :: S.Set Brick -> Brick -> S.Set Brick
getAboveBricks onGround b = S.filter (\x -> x /= b && x `directlyAbove` b) onGround

getBelowBricks :: S.Set Brick -> Brick -> S.Set Brick
getBelowBricks onGround b = S.filter (\x -> x /= b && b `directlyAbove` x) onGround

solveA :: [String] -> String
solveA input = show (S.size . getDeleteSet $ fallenBricks)
  where
    fallenBricks = turnOnGravity S.empty (sort bricks)
    bricks = parseInput input

solveB :: [String] -> String
solveB input = show $ sum $ S.foldr 
  (\b acc ->
    let newBricks = b `S.delete` fallenBricks; newFallenBricks = turnOnGravity S.empty (S.toAscList newBricks) 
    in S.size (S.filter (`S.notMember` fallenBricks) newFallenBricks) : acc) [] keepSet
  where
    keepSet = S.filter (`S.notMember` deleteSet) fallenBricks 
    deleteSet = getDeleteSet fallenBricks
    fallenBricks = turnOnGravity S.empty (sort bricks)
    bricks = parseInput input
    
parseInput :: [String] -> [Brick]
parseInput = map makeBrick

makeBrick :: String -> Brick
makeBrick input = if z1 > z2 then ptrace "snd z lower than fst z!" ((z1, x1, y1), (z2, x2, y2), -1) else ((z1, x1, y1), (z2, x2, y2), z1*1000+z2)
  where
    [begin, end] = splitOn "~" input
    [x1, y1, z1] = map read . splitOn "," $ begin
    [x2, y2, z2] = map read . splitOn "," $ end