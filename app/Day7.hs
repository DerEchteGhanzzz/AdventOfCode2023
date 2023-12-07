{-# LANGUAGE EmptyCase #-}
module Day7 (day7) where
import AocUtils
import Data.Map as M (insert, empty, toList, lookup)
import Data.List (sort, sortBy, sortOn)
import qualified Data.Ord
import Data.Ord (comparing)
import Debug.Trace
import Data.Char

day7 :: AocDay
day7 = MkDay 7 solveA solveB

toCard :: Char -> Card
toCard 'A' = A
toCard 'K' = K
toCard 'Q' = Q
toCard 'J' = J
toCard 'T' = T
toCard i   = if isDigit i then toEnum (read [i] - 1) else error "invalid card"

data Card = J | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | T | Q | K | A
  deriving (Show, Eq, Enum, Ord)

data Hand = MkHand {unhand :: [Card], getBid :: Int}
  deriving (Show, Eq)

instance Ord Hand where
  hand1 <= hand2 = if toHandtype hand1 == toHandtype hand2 then unhand hand1 < unhand hand2 else toHandtype hand1 < toHandtype hand2

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Show, Enum, Eq, Ord)

cardFreqs :: Hand -> [(Card, Int)]
cardFreqs hand = toList $ foldr (\card m -> case card `M.lookup` m of
  Just i -> insert card (i+1) m
  Nothing -> insert card 1 m) empty (unhand hand)

toHandtype :: Hand -> HandType
toHandtype hand | highestFreq    == 5 = FiveOfAKind
                | highestFreq    == 4 = FourOfAKind
                | length freqMap == 2 = FullHouse
                | highestFreq    == 3 = ThreeOfAKind
                | length freqMap == 3 = TwoPair
                | length freqMap == 4 = OnePair
                | highestFreq    == 1 = HighCard
                | otherwise = error $ show hand ++ ", " ++ show freqMap ++ " is not a valid hand"

  where
    freqMap' = reverse . sortOn snd . cardFreqs $ hand
    freqMap = removeJs freqMap' jAmt
    jAmt = length . filter (==J) $ unhand hand
    highestFreq = (snd . head) freqMap

removeJs :: [(Card, Int)] -> Int -> [(Card, Int)]
removeJs [] i = []
removeJs freqs 5 = freqs
removeJs ((J,f):freqs) i = removeJs freqs i
removeJs ((c,f):freqs) i = (c, f+i) : removeJs freqs 0

solveA :: [String] -> String
solveA = show . snd . foldl (\(idx, tot) h -> (idx+1, tot + (idx * getBid h))) (1, 0) . sort . parseInput

solveB :: [String] -> String
solveB input = "show input"

parseInput :: [String] -> [Hand]
parseInput = map (\line -> let [cards, bid] = words line in MkHand (map toCard cards) (read bid))

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (comparing Data.Ord.Down)