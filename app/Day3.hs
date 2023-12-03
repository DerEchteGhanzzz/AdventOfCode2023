module Day3 (day3) where
import AocUtils
import Data.List.Split
import Data.Char
import Data.Maybe
import Data.List
import Debug.Trace

type PartNumber = Int
type Schematic = [String]

data GearNumber = MkGN {value :: Int, getGears :: [(Int, Int)]}
  deriving (Show)

instance Eq GearNumber where
  (MkGN _ gears1) == (MkGN _ gears2) = any (`elem` gears2) gears1

multGearNumbers :: GearNumber -> GearNumber -> Int
multGearNumbers g1 g2 = value g1 * value g2

day3 :: AocDay
day3 = MkDay 3 solveA solveB

getPartNumbers :: [String] -> [PartNumber]
getPartNumbers s = getPartNumbers' (0, 0) s s

getPartNumbers' :: (Int, Int) -> Schematic -> Schematic -> [PartNumber]
getPartNumbers' _ _ [] = []
getPartNumbers' (r, c) scheme ([]:rows) = getPartNumbers' (0, c+1) scheme rows
getPartNumbers' (r, c) scheme ((ch:chs):rows) = case number of
  [] -> getPartNumbers' (r+1, c) scheme (chs:rows)
  n  -> case surroundingChars of
    "" -> getPartNumbers' (r+numlen, c) scheme (rest:rows)
    _  -> read n : getPartNumbers' (r+numlen, c) scheme (rest:rows)
  where
    surroundingChars = mapMaybe (\(x, y) -> 
      case getElem y x of
        Nothing -> Nothing
        Just symb -> if (not . isDigit) symb && symb /= '.' then Just symb else Nothing) 
      [(x, y) | y <- [c-1..c+1], x <- [r-1..r+numlen]]
    (number, rest) = span isDigit (ch:chs)
    numlen = length number
    getElem = getElem2D scheme

solveA :: [String] -> String
solveA = show . sum . getPartNumbers

solveB :: [String] -> String
solveB = show . sum . getGearRatios . getGearPositions

getGearRatios :: [GearNumber] -> [Int]
getGearRatios [] = []
getGearRatios (gn:gns) = case adjacent of
  [] -> getGearRatios gns
  [g] -> multGearNumbers gn g : getGearRatios filtered
  _   -> error "apparently there could be more"
  where
    adjacent = filter (==gn) gns
    filtered = filter (/=gn) gns

getGearPositions :: [String] -> [GearNumber]
getGearPositions s = getGearPositions' (0, 0) s s

getGearPositions' :: (Int, Int) -> Schematic -> Schematic -> [GearNumber]
getGearPositions' _ _ [] = []
getGearPositions' (r, c) scheme ([]:rows) = getGearPositions' (0, c+1) scheme rows
getGearPositions' (r, c) scheme ((ch:chs):rows) = case number of
  [] -> getGearPositions' (r+1, c) scheme (chs:rows)
  n  -> case surroundingGears of
    [] -> getGearPositions' (r+numlen, c) scheme (rest:rows)
    _  -> MkGN (read n) surroundingGears : getGearPositions' (r+numlen, c) scheme (rest:rows)
  where
    surroundingGears = filter (\(x, y) -> getElem y x == Just '*') [(x, y) | y <- [c-1..c+1], x <- [r-1..r+numlen]]
    (number, rest) = span isDigit (ch:chs)
    numlen = length number
    getElem = getElem2D scheme
    
