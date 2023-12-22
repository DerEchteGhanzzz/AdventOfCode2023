module Day19 (day19) where
import AocUtils

import Data.List.Split
import Data.Map as M
import Data.List as L
import Data.Char (isAlpha)
import Data.Maybe (fromJust)

day19 :: AocDay
day19 = MkDay 19 solveA solveB

---------------------------------------------------------------------------------------------------------------
-- PART A -----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

solveA :: [String] -> String
solveA input = show . sum . L.map (sum . snd) . L.filter ((=="A") . fst) . L.map (acceptParts workflows "in") $ parts
  where
    [workInput, partInput] = splitOn [""] input
    workflows = parseWorkflows workInput
    parts = parseParts partInput

type Part = Map String Int
type Workflow = [Part -> (Bool, String)]

acceptParts :: Map String Workflow -> String -> Part -> (String, Part)
acceptParts _ "A" part = ("A", part)
acceptParts _ "R" part = ("R", part)
acceptParts workflows current part = acceptParts workflows next part
  where
    ((_, next):_) = dropWhile (not . fst) (L.map (\w -> w part) workflow)
    workflow = fromJust $ M.lookup current workflows

---------------------------------------------------------------------------------------------------------------
-- PARSE A ----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

parseParts :: [String] -> [Part]
parseParts = L.map parsePart
  where
    parsePart part =
      let xmas = wordsBy (==',') . init . tail $ part
      in L.foldr (\crit m ->
        let [ch, v] = wordsBy (=='=') crit
        in M.insert ch (read v) m) M.empty xmas

parseWorkflows :: [String] -> Map String Workflow
parseWorkflows = L.foldr (\workflow m -> let (name, rest) = span isAlpha workflow in M.insert name (parseCrits rest) m) M.empty
  where
    parseCrits crits =
      let critList = wordsBy (==',') . init . tail $ crits
          in
            L.map (\critStr ->
              let (name, pred) = span isAlpha critStr
              in case pred of
                [] -> const (True, name)
                _  -> makeBinding name pred) critList
    makeBinding name predStr = \m -> let v = fromJust $ M.lookup name m in (toOp op v (read target), next)
      where
        toOp o = case o of
          "<" -> (<)
          ">" -> (>)
          _   -> error $ "invalid operator: " ++ show o
        (op, targetNext) = L.splitAt 1 predStr
        [target, next] = wordsBy (==':') targetNext

---------------------------------------------------------------------------------------------------------------
-- PART B -----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

solveB :: [String] -> String
solveB input = show . acceptRangeParts workflows "in" $ rangedPart
  where
    [workInput, _] = splitOn [""] input
    workflows = parseWorkflows' workInput
    rangedPart = M.fromList $ L.map (\ch -> ([ch], (1, 4000))) "xmas"

type RangedWorkflow = [(String, String, Int, String)]

type Range = (Int, Int)
type RangedPart = Map String Range

acceptRangeParts :: Map String RangedWorkflow -> String -> RangedPart -> Int
acceptRangeParts _ "A" part = product . M.map (\(b, e) -> e - b + 1) $ part
acceptRangeParts _ "R" _    = 0
acceptRangeParts workflows current rPart = sum . L.map (uncurry (acceptRangeParts workflows)) . L.foldl (\parts crit -> L.concatMap (splitPart crit) parts) [("", rPart)] $ workflow
  where
    workflow = case M.lookup current workflows of
      Just x -> x
      _ -> error $ show current

splitPart :: (String, String, Int, String) -> (String, RangedPart) -> [(String, RangedPart)]
splitPart (a, p, t, n) ("", part) | (p == "<" && snd partValue < t) || (p == ">" && fst partValue > t) = [(n, part)]
                                  | (p == "<" && fst partValue >= t) || (p == ">" && snd partValue <= t) = [("", part)]
                                  | p == "<" = [(n, M.insert a (fst partValue, t-1) part), ("", M.insert a (t, snd partValue) part)]
                                  | p == ">" = [("", M.insert a (fst partValue, t) part), (n, M.insert a (t+1, snd partValue) part)]
                                  | otherwise = error $ show (a, p, t, n, part)
  where
    partValue = fromJust $ M.lookup a part
splitPart _ cp = [cp]

---------------------------------------------------------------------------------------------------------------
-- PARSE B ----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------

parseWorkflows' :: [String] -> Map String RangedWorkflow
parseWorkflows' = L.foldr (\workflow m -> let (name, rest) = span isAlpha workflow in M.insert name (parseCrits rest) m) M.empty
  where
    parseCrits crits =
      let critList = wordsBy (==',') . init . tail $ crits
          in
            L.map (\critStr ->
              let (name, pred) = span isAlpha critStr
              in case pred of
                [] -> ("x", "<", 4001, name)
                _  -> makeBinding name pred) critList
    makeBinding name predStr = (name, op, read target, next)
      where
        (op, targetNext) = L.splitAt 1 predStr
        [target, next] = wordsBy (==':') targetNext