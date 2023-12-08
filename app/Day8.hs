module Day8 (day8) where
import AocUtils
import Data.Map as M (Map, insert, empty, lookup, toList)
import Data.List.Split (splitOn)
import Data.Char (isAlpha, isDigit)
import Data.Maybe (fromJust)

type Node = String
type LRMap = Map Node (Node, Node)
type LRInstr = String

day8 :: AocDay
day8 = MkDay 8 solveA solveB

performInstructions :: Int -> LRInstr -> LRInstr -> [Node] -> Int -> Char -> LRMap -> Int
performInstructions steps is lrI nodes cycles end lrMap
  | all (\n -> last n == end) nodes && steps /= cycles = steps
  | otherwise = case is of
    [] -> performInstructions steps lrI lrI nodes cycles end lrMap
    (i:is) -> performInstructions steps' is lrI nodes' cycles end lrMap
      where
        nodes' = map newNode nodes
        tup n = M.lookup n lrMap
        newNode n = case i of
              'L' -> (fst . fromJust) (tup n)
              'R' -> (snd . fromJust) (tup n)
              _   -> error $ show i ++ " is not a valid instruction."
    where
      steps' = steps + 1

lcms :: [Int] -> Int
lcms (i:is) = foldl lcm i is

solveA :: [String] -> String
solveA input = show $ performInstructions 0 instr instr ["AAA"] 0 'Z' lrMap
  where
    (instr, lrMap) = parseInput input

solveB :: [String] -> String
solveB input = (show . lcms) ends
  where
    ends = map (\n -> performInstructions 0 instr instr [n] 0 'Z' lrMap) getANodes
    getANodes = foldr (\(n, _) l -> if last n == 'A' then n:l else l) [] (toList lrMap)
    (instr, lrMap) = parseInput input

parseInput :: [String] -> (LRInstr, LRMap)
parseInput (instruction:_:input) = (instruction, makeMap input)

makeMap :: [String] -> LRMap
makeMap = foldr (\line m ->
  let [start, tup] = splitOn " = " line;
      [l, r] = map (filter (\x -> isAlpha x || isDigit x)) . words $ tup
  in
    insert start (l, r) m) empty