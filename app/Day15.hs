module Day15 (day15) where
import AocUtils

import Data.Char
import Data.List.Split (wordsBy)
import Data.Map as M (insert, empty, lookup, toList)
import Data.Maybe (fromJust)

type Lens = (String, Int)
type Box = [Lens]

type BoxOp = Lens -> Box -> Box

day15 :: AocDay
day15 = MkDay 15 solveA solveB

solveA :: [String] -> String
solveA = show . sum . map encrypt . wordsBy (==',') . head

solveB :: [String] -> String
solveB input = show . snd . foldl (\(idx, tot) b -> (idx + 1, tot + calcLensPower idx b)) (0, 0) . foldl insertInBoxes boxes $ steps
  where
    steps = wordsBy (==',') . head $ input
    boxes = map (const []) [0..255]

calcLensPower :: Int -> Box -> Int
calcLensPower = calcLensPower' 1
  where
    calcLensPower' :: Int -> Int -> Box -> Int
    calcLensPower' idx n [] = 0
    calcLensPower' idx n (x:xs) = (n+1) * idx * snd x + calcLensPower' (idx+1) n xs

encrypt :: String -> Int
encrypt = foldl (\tot ch -> ((tot + ord ch) * 17) `mod` 256) 0

removeLens :: Lens -> Box -> Box
removeLens lens [] = []
removeLens lens (x:xs) | fst lens == fst x = xs
                       | otherwise = x : lens `removeLens` xs

insertLens :: Lens -> Box -> Box
insertLens lens [] = [lens]
insertLens lens (x:xs) | fst lens == fst x = lens:xs
                       | otherwise = x : lens `insertLens` xs

insertInBoxes :: [Box] -> String -> [Box]
insertInBoxes boxes lensStr = insertInBoxes' 0 n op (label, i) boxes
  where
    (label, power) = span isAlpha lensStr
    (i, op)    =
      if '-' `elem` lensStr 
        then (-1, removeLens) 
        else (read $ dropWhile (not . isDigit) power, insertLens)
    n = encrypt label

insertInBoxes' :: Int -> Int -> BoxOp -> Lens -> [Box] -> [Box]
insertInBoxes' _ n _ _ [] = error $ "box "++ show n ++ "not found."
insertInBoxes' idx n o l (b:bs) | idx == n = o l b : bs
                                | otherwise = b : insertInBoxes' (idx+1) n o l bs