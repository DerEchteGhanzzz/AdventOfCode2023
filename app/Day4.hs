module Day4 (day4) where
import AocUtils
import Data.List.Split (splitOn)
import Data.List.Utils (replace)

day4 :: AocDay
day4 = MkDay 4 solveA solveB

data ScratchCard = MkSC {cardNum :: Int, myNums :: [Int], winningNums :: [Int]}
  deriving (Show)

instance Eq ScratchCard where
  sc1 == sc2 = cardNum sc1 == cardNum sc2

instance Ord ScratchCard where
  sc1 <= sc2 = cardNum sc1 <= cardNum sc2

calculateScore :: ScratchCard -> Int
calculateScore sc = case winningAmt sc of
  0 -> 0
  i -> 2 ^ (i - 1)

winningAmt :: ScratchCard -> Int
winningAmt sc = length $ filter (\x -> x `elem` winningNums sc) $ myNums sc

calculateNewCards :: [(ScratchCard, Int)] -> [(ScratchCard, Int)]
calculateNewCards [] = []
calculateNewCards ((card, amt):cards) = (card, amt) : calculateNewCards (map (\(sc, i) -> (sc, i+amt)) winners ++ rest)
  where
    (winners, rest) = splitAt (winningAmt card) cards

solveA :: [String] -> String
solveA = show . sum . map (calculateScore . fst) . parseInput

solveB :: [String] -> String
solveB = show . foldr ((+) . snd) 0 . calculateNewCards .  parseInput

parseInput :: [String] -> [(ScratchCard, Int)]
parseInput = map toScratchCard

toScratchCard :: String -> (ScratchCard, Int)
toScratchCard s = (MkSC (read number) (map read myNumbers) (map read . concat $ winners), 1)
  where
    (card:number:nums) = words . replace ":" "" $ s
    (myNumbers:winners) = splitOn ["|"] nums
