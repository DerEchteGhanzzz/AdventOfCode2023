module Day7 (day7) where
import AocUtils
import Data.Map as M (insert, empty, toList, lookup)
import Data.List (sort, sortBy, sortOn)
import qualified Data.Ord
import Data.Ord (comparing)
import Data.Char
import Data.List.Utils (replace)

day7 :: AocDay
day7 = MkDay 7 solveA solveB

data Card = W | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | T | J | Q | K | A
  deriving (Show, Eq, Enum, Ord)

toCard :: Char -> Card
toCard 'A' = A
toCard 'K' = K
toCard 'Q' = Q
toCard 'J' = J
toCard 'T' = T
toCard 'W' = W
toCard i   = if isDigit i then toEnum (read [i] - 1) else error "invalid card"

data Hand = MkHand {unhand :: [Card], getBid :: Int}
  deriving (Show, Eq)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Show, Enum, Eq, Ord)

instance Ord Hand where
  hand1 <= hand2 = (toHandtype hand1, unhand hand1) <= (toHandtype hand2, unhand hand2)

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
    freqMap = removeWilds wildcardAmt . sortOn (Data.Ord.Down . snd) . cardFreqs $ hand
    wildcardAmt = length . filter (==W) $ unhand hand
    highestFreq = (snd . head) freqMap

removeWilds :: Int -> [(Card, Int)] -> [(Card, Int)]
removeWilds 0 freqs = freqs
removeWilds 5 freqs = freqs
removeWilds i [] = []
removeWilds i ((W,f):freqs) = removeWilds i freqs
removeWilds i ((c,f):freqs) = (c, f+i) : filter ((/=W) . fst) freqs

solveA :: [String] -> String
solveA = show . snd . foldl (\(idx, tot) h -> (idx+1, tot + (idx * getBid h))) (1, 0) . sort . parseInput

solveB :: [String] -> String
solveB = show . snd . foldl (\(idx, tot) h -> (idx+1, tot + (idx * getBid h))) (1, 0) . sort . map (\h -> h {unhand = replace [J] [W] (unhand h)}) . parseInput

parseInput :: [String] -> [Hand]
parseInput = map (\line -> let [cards, bid] = words line in MkHand (map toCard cards) (read bid))