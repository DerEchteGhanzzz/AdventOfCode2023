module Day3 (day3) where
import AocUtils
import Data.List.Split
import Data.Char
import Data.Maybe
import Data.List
import Debug.Trace

day3 :: AocDay
day3 = MkDay 3 solveA solveB

type Schematic = [String]

data Part = MkPart {getPartChar :: Char, getCoord :: (Int, Int)}
  deriving (Eq, Show)

data PartNumber = MkPN {value :: Int, getParts :: [Part]}
  deriving (Show)

instance Eq PartNumber where
  (MkPN _ gears1) == (MkPN _ gears2) = any (`elem` gears2) gears1

multGearNumbers :: PartNumber -> PartNumber -> Int
multGearNumbers g1 g2 = value g1 * value g2

getPartNumbers :: [String] -> [PartNumber]
getPartNumbers s = getPartNumbers' (0, 0) s s

getPartNumbers' :: Point -> Schematic -> Schematic -> [PartNumber]
getPartNumbers' _ _ [] = []
getPartNumbers' pos@(r, c) scheme ([]:rows) = getPartNumbers' (0, c+1) scheme rows
getPartNumbers' pos@(r, c) scheme ((ch:chs):rows) = case number of
  [] -> getPartNumbers' (r+1, c) scheme (chs:rows)
  n  -> case surroundingParts of
    [] -> getPartNumbers' (r+numlen, c) scheme (rest:rows)
    _  -> MkPN (read n) surroundingParts : getPartNumbers' (r+numlen, c) scheme (rest:rows)
  where
    surroundingParts = mapMaybe (\(x, y) ->
      case getElem2D scheme (x, y) of
        Nothing -> Nothing
        Just symb -> if (not . isDigit) symb && symb /= '.' then Just (MkPart symb (x, y)) else Nothing)
      [(x, y) | y <- [c-1..c+1], x <- [r-1..r+numlen]]
    (number, rest) = span isDigit (ch:chs)
    numlen = length number

getGearPairs :: [PartNumber] -> [(PartNumber, PartNumber)]
getGearPairs = getGearPairs' . filter (any ((=='*') . getPartChar) . getParts)

getGearPairs' :: [PartNumber] -> [(PartNumber, PartNumber)]
getGearPairs' [] = []
getGearPairs' (gn:gns) = case adjacent of
  Nothing  -> getGearPairs' gns
  Just gn2 -> (gn, gn2) : getGearPairs' rest
  where
    (adjacent, rest) = foldr (\gn2 (adj, filt) -> if gn2 == gn then (Just gn2, filt) else (adj, gn2:filt)) (Nothing, []) gns

solveA :: [String] -> String
solveA = show . foldr ((+) . value) 0 . getPartNumbers

solveB :: [String] -> String
solveB = show . foldr ((+) . uncurry multGearNumbers) 0 . getGearPairs . getPartNumbers