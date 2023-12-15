module Day12 (day12) where
import AocUtils
import Data.List.Split (splitOn, wordsBy, splitWhen)
import Data.Char
import Data.Map as M
import Data.List as L
import Data.List.Utils (replace)
import Data.Function (fix)
import Data.Function.Memoize

day12 :: AocDay
day12 = MkDay 12 solveA solveB

calcCombs :: (String, [Int]) -> Int
calcCombs = fst . calcCombs' M.empty 0 0 0

charAmt :: Eq a => [a] -> a -> Int
charAmt str c = length . L.filter (==c) $ str

calcCombs' :: Map (Int, Int, Int) Int -> Int -> Int -> Int -> (String, [Int]) -> (Int, Map (Int, Int, Int) Int) 
calcCombs' memoMap idx critIdx hashHad (row, crits) = case mt `M.lookup` memoMap of
  Just n -> (n, memoMap)
  Nothing | critIdx == length crits -> if row `charAmt` '#' == 0 then (1, M.insert mt 1 memoMap) else (0, M.insert mt 0 memoMap)
          | L.null row -> if critIdx == length crits - 1 && hashHad == crits !! critIdx then (1, M.insert mt 1 memoMap) else (0, M.insert mt 0 memoMap)
          | head row == '?' -> if hashHad == crits !! critIdx then calcCombs' memoMap idx' critIdx' 0 (tail row, crits) else
            if hashHad < crits !! critIdx 
              then let (r, memoMap') = calcCombs' memoMap idx' critIdx hashHad' (tail row, crits) in 
                if hashHad == 0 
                  then let (l, memoMap'') = calcCombs' memoMap' idx' critIdx hashHad (tail row, crits) in 
                    (r + l, M.insert mt (r+l) memoMap'')
                  else (r, M.insert mt r memoMap')
              else (0, M.insert mt 0 memoMap)
          | head row == '#' -> 
            if hashHad' > crits !! critIdx 
              then (0, M.insert mt 0 memoMap) 
              else let (m, memoMap') = calcCombs' memoMap idx' critIdx hashHad' (tail row, crits) in 
                (m, M.insert mt m memoMap')
          | hashHad /= 0 && hashHad /= crits !! critIdx -> (0, M.insert mt 0 memoMap)
          | hashHad == crits !! critIdx -> let (m, memoMap') = calcCombs' memoMap idx' critIdx' 0 (tail row, crits) in (m, M.insert mt m memoMap')
          | otherwise -> let (m, memoMap') = calcCombs' memoMap idx' critIdx 0 (tail row, crits) in (m, M.insert mt m memoMap')
  where
    mt = (idx, critIdx, hashHad)
    idx' = idx + 1
    hashHad' = hashHad + 1
    critIdx' = critIdx + 1

solveA :: [String] -> String
solveA = show . sum . L.map calcCombs . parseInput

solveB :: [String] -> String
solveB input = show . sum $ largeAnswer
  where
    expand (row, ints) = (row++'?':row++'?':row++'?':row++'?':row, concat $ replicate 5 ints)
    largeAnswer = L.map (calcCombs . expand) . parseInput $ input

parseInput :: [String] -> [(String, [Int])]
parseInput = L.map (\l -> let [row, counts] = words l in (row, L.map read $ splitOn "," counts))