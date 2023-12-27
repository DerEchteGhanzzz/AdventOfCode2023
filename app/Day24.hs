module Day24 (day24) where
import AocUtils
import Data.Tuple.Utils (snd3, fst3)

day24 :: AocDay
day24 = MkDay 24 solveA solveB

trd :: (a, b, c) -> c
trd (_, _, c) = c

type Ray2D = ((Double, Double), (Double, Double, Double, Double))
type Box2D = (Double, Double)
intersectsWithin2DBox :: [Ray2D] -> Box2D -> Ray2D -> [Bool]
intersectsWithin2DBox rs box r = map (`intersectWithin2DBox` r) rs
  where
    intersectWithin2DBox ((a, b), (vx1, vy1, px1, py1)) ((c, d), (vx2, vy2, px2, py2))
      | a == c = False
      | otherwise =
        let
          x = (d - b) / (a - c)
          y = a*x + b
        in
           x * signum vx1 >= px1 * signum vx1 &&
           x * signum vx2 >= px2 * signum vx2 &&
           y * signum vy1 >= py1 * signum vy1 &&
           y * signum vy2 >= py2 * signum vy2 &&
          inside2D (x, y) box
    inside2D (x, y) (minXY, maxXY) = minXY <= x && x <= maxXY && minXY <= y && y <= maxXY

solveA :: [String] -> String
solveA input = show . (`div` 2) . length . concatMap (filter id . intersectsWithin2DBox rays box) $ rays
  where
    rays = parseInput2D input
    box = (200000000000000, 400000000000000)


findParallels :: [Ray3D] -> Ray3D -> [Ray3D]
findParallels rs ((_, vx), (_, vy), (_, vz)) = filter (\((_, vx2), (_, vy2), (_, vz2)) -> vx/vy == vx2/vy2 && vx/vz == vx2/vz2) rs


solveB :: [String] -> String
solveB input = show . length . filter ((>=2) . length). map (findParallels rays) $ rays
  where
    rays = parseInput3D input

parseInput2D :: [String] -> [Ray2D]
parseInput2D = map (\l -> let [px, py, _, vx, vy, _] = words . filter (`notElem` ",@") $ l in to2DLine (read px) (read py) (read vx) (read vy))

to2DLine :: Double -> Double -> Double -> Double -> Ray2D
to2DLine px py vx vy = ((a, b), (vx, vy, px, py))
  where
    a = vy / vx
    b = py - (a * px)

type Ray3D = ((Double, Double), (Double, Double), (Double, Double))

parseInput3D :: [String] -> [Ray3D]
parseInput3D = map (\l -> let [px, py, pz, vx, vy, vz] = words . filter (`notElem` ",@") $ l in ((read px, read vx), (read py, read vy), (read pz, read vz)))

getCrossingLine :: [Ray3D] -> Ray3D
getCrossingLine (((px1, vx1), (py1, vy1), (pz1, vz1)):rs) = getVels pxyz0s rs
  where
    pxyz0s = [(px1-t*vx1, py1-t*vy1, pz1-t*vz1) | t <- [0,1..]]

getVels :: [(Double, Double, Double)] -> [Ray3D] -> Ray3D
getVels ((px0, py0, pz0):starts) (((px2, vx2), (py2, vy2), (pz2, vz2)):rs) = crossNext starts vxyz0s ((px0, vx0), (py0, vy0), (pz0, vz0)) rs
  where
    ((vx0, vy0, vz0):vxyz0s) = [(px0-px2+vx2*t, py0-py2+vy2*t, pz0-pz2+vz2*t) | t <- [0,1..]]

crossNext starts velocities ((px0, vx0), (py0, vy0), (pz0, vz0)) = undefined
