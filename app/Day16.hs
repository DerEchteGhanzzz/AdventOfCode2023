module Day16 (day16) where
import AocUtils

import Data.Map as M
import Data.List as L
import Data.Set as S
import Data.Maybe

data Direction = RIGHT | LEFT | UP | DOWN
  deriving (Eq, Show, Ord)

day16 :: AocDay
day16 = MkDay 16 solveA solveB

solveA :: [String] -> String
solveA input = show . L.foldr (\d t -> if L.null d then t else t+1) (-1) $ traverseMirror'
  where
    traverseMirror' = traverseMirror mirrorMap (-1,0) RIGHT excitedStates
    excitedStates = M.insert (-1,0) S.empty $ M.map (const S.empty) mirrorMap
    mirrorMap = parseInput input

solveB :: [String] -> String
solveB input = show . maximum . L.map (L.foldr (\d t -> if L.null d then t else t+1) (-1)) $ allStarts
  where
    startsRight = L.map (\i -> traverseMirror mirrorMap (-1, i) RIGHT (M.insert (-1, i) S.empty excitedStates)) [0..109]
    startsLeft = L.map (\i -> traverseMirror mirrorMap (110, i) LEFT (M.insert (110, i) S.empty excitedStates)) [0..109]
    startsBot = L.map (\i -> traverseMirror mirrorMap (i, 110) UP (M.insert (i, 110) S.empty excitedStates)) [0..109]
    startsTop = L.map (\i -> traverseMirror mirrorMap (i, -1) DOWN (M.insert (i, -1) S.empty excitedStates)) [0..109]
    allStarts = startsBot ++ startsLeft ++ startsRight ++ startsTop
    excitedStates = M.map (const S.empty) mirrorMap
    mirrorMap = parseInput input

parseInput :: [String] -> Map Point Char
parseInput i = snd $ L.foldl (\(y, m) row -> (y + 1, snd $ L.foldl (\(x, m') ch -> (x + 1, M.insert (x, y) ch m')) (0, m) row)) (0, M.empty) i

traverseMirror :: Map Point Char -> Point -> Direction -> Map Point (Set Direction) -> Map Point (Set Direction)
traverseMirror mirrorMap pos@(x,y) dir excitedStates =
  case M.lookup pos excitedStates of
    Nothing -> error "kijk hier!"
    Just ds | dir `S.member` ds -> excitedStates
    Just ds ->
      case M.lookup pos' mirrorMap of
        Just '.' -> traverseMirror mirrorMap pos' dir excitedStates'
        Just ch | ch == '/' || ch == '\\' -> traverseMirror mirrorMap pos' (dir `handleMirror` ch) excitedStates'
                | isSplit ch dir -> let leftBranch = traverseMirror mirrorMap pos' dirLeft excitedStates' in 
                  traverseMirror mirrorMap pos' dirRight (M.unionWith S.union leftBranch excitedStates')
                | otherwise -> traverseMirror mirrorMap pos' dir excitedStates'
        Nothing -> excitedStates'
        where
          (dirLeft, dirRight) = getSplitDirs dir
          pos' = updatePos pos dir
          excitedStates' = M.insert pos (S.insert dir ds) excitedStates

isSplit :: Char -> Direction -> Bool
isSplit '|' dir = case dir of
  RIGHT -> True
  LEFT -> True
  _ -> False
isSplit '-' dir = case dir of
  UP -> True
  DOWN -> True
  _ -> False

getSplitDirs :: Direction -> (Direction, Direction)
getSplitDirs RIGHT = (UP, DOWN)
getSplitDirs LEFT = (DOWN, UP)
getSplitDirs UP = (LEFT, RIGHT)
getSplitDirs DOWN = (LEFT, RIGHT)

handleMirror :: Direction -> Char -> Direction
handleMirror RIGHT '/' = UP
handleMirror LEFT '/'  = DOWN
handleMirror UP '/'    = RIGHT
handleMirror DOWN '/'  = LEFT
handleMirror RIGHT '\\' = DOWN
handleMirror LEFT '\\'  = UP
handleMirror UP '\\'    = LEFT
handleMirror DOWN '\\'  = RIGHT
handleMirror _ ch = error $ "invalid mirror" ++ show ch

updatePos :: Point -> Direction -> Point
updatePos (x, y) RIGHT = (x+1, y)
updatePos (x, y) LEFT  = (x-1, y)
updatePos (x, y) DOWN  = (x, y+1)
updatePos (x, y) UP    = (x, y-1)

printMap :: Map Point (Set Direction) -> [String]
printMap m = L.map (\x -> ptrace x x) stringMap
  where
  stringMap = L.foldr (\y tot -> (show y ++ L.foldr (\x tot' -> case M.lookup (x, y) m of
    Just s | not . S.null $ s-> '#':tot'
    _ -> '.':tot') "" [0..109]) : tot) [] [0..109]