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

setSum :: CubeSet -> (Int, Int, Int)
setSum =
  foldr
    ( \cube (r, g, b) ->
        case getColor cube of
          Red -> (r + getAmount cube, g, b)
          Green -> (r, g + getAmount cube, b)
          Blue -> (r, g, b + getAmount cube)
    )
    (0, 0, 0)

isValidGame :: Game -> Int -> Int -> Int -> Bool
isValidGame game red green blue = all (checkIfEnough . setSum) (getCubeSets game)
  where
    checkIfEnough (r, g, b) = r <= red && g <= green && b <= blue

getValidGames :: [Game] -> [Game]
getValidGames = foldr (\game acc -> if isValidGame game 12 13 14 then game : acc else acc) []

minSetPower :: Game -> Int
minSetPower game = tupleMult . foldr (\cset acc -> tupCompare acc (setSum cset)) (0, 0, 0) $ getCubeSets game
  where
    tupleMult (a, b, c) = a * b * c
    tupCompare (a, b, c) (x, y, z) = (max a x, max b y, max c z)

solveA :: [String] -> String
solveA input = show $ foldr (\game acc -> acc + getID game) 0 $ getValidGames $ parseInput input

solveB :: [String] -> String
solveB input = show $ foldr (\game acc -> acc + minSetPower game) 0 $ parseInput input

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