{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Day20 (day20) where
import AocUtils
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, fromJust)

day20 :: AocDay
day20 = MkDay 20 solveA solveB

data Signal = High | Low
  deriving (Eq, Show, Ord)

invSig :: Signal -> Signal
invSig High = Low
invSig Low = High

flpFlp :: Signal -> Power -> (Power, Maybe Signal)
flpFlp High p = (p, Nothing)
flpFlp Low  On = (Off, Just Low)
flpFlp Low Off = (On, Just High)

type Pulse = (String, String, Signal)
data Power = On | Off
  deriving (Eq, Show, Ord)
data Radiotower = FlipFlop {flpflpPower :: Power, getDests :: [String], getName :: String}
                | Conjuction {conjMem :: M.Map String Signal, getDests ::[String], getName :: String}
                | Broadcaster {getDests :: [String], getName :: String}
                | Out {getPower :: Power, getDests :: [String], getName :: String}
                deriving (Eq, Show, Ord)

lcmm :: Integral a => [a] -> a
lcmm = foldr lcm 1

solveA :: [String] -> String
solveA input = show (h * l)
  where
    ((h, l), counterMap, rtMap') = pushNTimes rtMap 0 1000 (0, 0) (M.map (const 0) $ conjMem $ fromJust $ M.lookup "hb" rtMap)
    rtMap = parseInput input

solveB :: [String] -> String
solveB input = show (h * l)
  where
    ((h, l), counterMap, rtMap') = pushNTimes rtMap 0 (-1) (0, 0) (M.map (const 0) $ conjMem $ fromJust $ M.lookup "hb" rtMap)
    rtMap = parseInput input

pushNTimes :: M.Map String Radiotower -> Int -> Int -> Point ->  M.Map String Int -> (Point, M.Map String Int, M.Map String Radiotower)
pushNTimes rtMap count maxAmt total m = if count == maxAmt then (total, m, rtMap) 
  else (total, m, rtMap) --pushNTimes rtMap' (count + 1) maxAmt (fst total' + fst total, snd total' + snd total) m
    where
      (total', rtMap') = pushButton rtMap (count + 1)

hasAllHigh :: M.Map String Int -> Bool
hasAllHigh m = 0 /= M.foldr (*) 1 m

flpOnAmt :: M.Map String Radiotower -> (Int, Int)
flpOnAmt = M.foldr (\rt (i, c) -> case rt of
  FlipFlop {} -> if flpflpPower rt == On then (i + 1, c) else (i, c)
  Conjuction {} -> (i, if all ((==High) . snd) (M.toList $ conjMem rt) then c + 1 else c)
  _ -> (i, c)) (0, 0)

pushButton :: M.Map String Radiotower -> Int -> ((Int, Int), M.Map String Radiotower)
pushButton rtMap c = sendPulses c rtMap [("", "broadcaster", Low)] (0, 1)

sendPulses :: (Num a, Num b) => Int -> M.Map [Char] Radiotower -> [Pulse] -> (a, b) -> ((a, b), M.Map [Char] Radiotower)
sendPulses c rtState pulses hlAmts | null pulses = (hlAmts, rtState)
                                 | otherwise = sendPulses c rtState' pulses' hlAmts'
  where
    getNewPulses :: String -> [String] -> Maybe Signal -> [Pulse]
    getNewPulses _ _ Nothing = []
    getNewPulses orig dests (Just sig) = map (\d -> (orig, d, sig)) dests
    hlAmts' = foldr (\(_, _, s) (h, l) -> if s == High then (h + 1, l) else (h, l + 1)) hlAmts pulses'
    (rtState', pulses') = foldl (\(rtS, ps) (orig, dest, sig) -> case M.lookup dest rtS of
      Nothing -> (rtS, ps)
      Just rt -> case rt of
        Broadcaster {} -> (rtS, ps ++ getNewPulses dest (getDests rt) (Just sig))
        FlipFlop {}    -> let (power', newSignal) = flpFlp sig $ flpflpPower rt in (M.insert dest (rt {flpflpPower = power'}) rtS, ps ++ getNewPulses dest (getDests rt) newSignal)
        Conjuction {}  -> let rt' = rt {conjMem = M.insert orig sig (conjMem rt)}; sigToSend = if all ((==High) . snd) (M.toList $ conjMem rt') then Low else High
                            in (M.insert dest rt' rtS, ps ++ getNewPulses dest (getDests rt) (Just sigToSend))
        Out {}         -> let power' = if sig == Low then On else getPower rt in (M.insert dest (rt {getPower = power'}) rtS, ps)
        ) (rtState, []) pulses

parseInput :: [String] -> M.Map String Radiotower
parseInput input = foldr (\(name, rt) rtM -> case rt of
  Conjuction {} -> M.insert name (rt {conjMem = M.foldr (\r i -> if name `elem` getDests r then M.insert (getName r) Low i else i) (conjMem rt) rtM}) rtM
  _ -> rtM
  ) rtMap (M.toList rtMap)
  where
    rtMap = M.insert "rx" (Out Off [] "rx") $ foldr (\l m -> let (n, rt) = parseLine l in M.insert n rt m) M.empty input

parseLine :: String -> (String, Radiotower)
parseLine ('%':rest) = let (name:_:dests) = words . filter (/=',') $ rest in (name, FlipFlop Off dests name)
parseLine ('&':rest) = let (name:_:dests) = words . filter (/=',') $ rest in (name, Conjuction M.empty dests name)
parseLine rest       = let (name:_:dests) = words . filter (/=',') $ rest in (name, Broadcaster dests name)