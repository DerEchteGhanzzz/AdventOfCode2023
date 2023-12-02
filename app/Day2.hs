module Day2 (day2) where

import AocUtils
import Data.List.Split

data Color = Red | Blue | Green
  deriving (Eq, Show)

data Cube = MkCube {getAmount :: Int, getColor :: Color}
  deriving (Eq, Show)

type CubeSet = [Cube]

data Game = MkGame {getID :: Int, getCubeSets :: [CubeSet]}
  deriving (Eq, Show)

day2 :: AocDay
day2 = MkDay 2 solveA solveB

cubeToTriple :: Cube -> (Int, Int, Int)
cubeToTriple (MkCube i Red)   = (i, 0, 0)
cubeToTriple (MkCube i Green) = (0, i, 0)
cubeToTriple (MkCube i Blue)  = (0, 0, i)

setSum :: CubeSet -> (Int, Int, Int)
setSum = foldr (tripAdd . cubeToTriple) (0, 0, 0)
  where
    tripAdd (a, b, c) (x, y, z) = (a + x, b + y, c + z)

isValidGame :: Int -> Int -> Int -> Game -> Bool
isValidGame maxRed maxGreen maxBlue game = all (checkIfEnough . setSum) (getCubeSets game)
  where
    checkIfEnough (r, g, b) = r <= maxRed && g <= maxGreen && b <= maxBlue

minSetPower :: Game -> Int
minSetPower game = tripMult . foldr (tripMax . setSum) (0, 0, 0) $ getCubeSets game
  where
    tripMult (a, b, c) = a * b * c
    tripMax (a, b, c) (x, y, z) = (max a x, max b y, max c z)

solveA :: [String] -> String
solveA input = show . sum . map getID . filter (isValidGame 12 13 14) $ parseInput input

solveB :: [String] -> String
solveB input = show . sum . map minSetPower $ parseInput input

---------------------------------------------------------------------------------------------------------------------
-- PARSING ----------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------

parseInput :: [String] -> [Game]
parseInput = map (parseGame . splitOn ": ")

parseGame :: [String] -> Game
parseGame [id, sets] = MkGame (read (last $ words id)) (map parseSet (splitOn "; " sets))

parseSet :: String -> CubeSet
parseSet str = map (parseCube . words) (splitOn ", " str)

parseCube :: [String] -> Cube
parseCube [i, "blue"] = MkCube (read i) Blue
parseCube [i, "red"] = MkCube (read i) Red
parseCube [i, "green"] = MkCube (read i) Green
parseCube x = error $ show x ++ " is an invalid cube."