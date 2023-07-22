{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Days.Day6 (solve) where
import           AoCUtils.Days  (Solver)

import           AoCUtils.Regex (parseUnsignedInts)
import           Data.Foldable  (maximumBy, minimumBy)
import           Data.Ord       (comparing)
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Utils.Graphs   (BFSOptions (BFSOptions, keepVisited, pruneFun),
                                 reachableBFS, reachableBFSLimit)

type Pos = (Int, Int)

solve :: Solver
solve input = (show part1, show part2)
  where
    coords = Set.fromList $ map parseCoord $ lines input
    part1 = solve1 coords
    part2 = solve2 coords

-- Parsing

parseCoord :: String -> Pos
parseCoord str = (tokens !! 0, tokens !! 1)
  where
    tokens = parseUnsignedInts str

-- Part 1

solve1 :: Set Pos -> Int
solve1 coords = maximum $ filter (< limit) $ map (closestArea coords limit) coords'
  where
    limit = findLimit coords
    coords' = Set.toList coords

closestArea :: Set Pos -> Int -> Pos -> Int
closestArea coords limit current = Set.size $ reachableBFSLimit current limit adjacency options
  where
    options = BFSOptions {keepVisited = True, pruneFun = Nothing}
    adjacency = areaAdjecency coords current

findLimit :: Set Pos -> Int
findLimit coords = (maxX - minX) * (maxY - minY) `div` 10
  where
    (minX, maxX, minY, maxY) = findDimensions coords

areaAdjecency :: Set Pos -> Pos -> Pos -> [Pos]
areaAdjecency coords current pos = filter (isCloserTo current coords) neighbors
  where
    neighbors = getNeighbors pos

isCloserTo :: Pos -> Set Pos -> Pos -> Bool
isCloserTo target coords pos =
  all (\coord -> distance pos target < distance pos coord) coords'
  where
    coords' = Set.delete target coords

-- Part 2

solve2 :: Set Pos -> Int
solve2 coords = Set.size $ reachableBFS start adjacency options
  where
    start = findMiddle coords
    adjacency = regionAdjacency coords
    options = BFSOptions {keepVisited = True, pruneFun = Nothing}

findMiddle :: Set Pos -> Pos
findMiddle coords = (x, y)
  where
    (minX, maxX, minY, maxY) = findDimensions coords
    x = (minX + maxX) `div` 2
    y = (minY + maxY) `div` 2

regionAdjacency :: Set Pos -> Pos -> [Pos]
regionAdjacency coords pos = filter (safe coords) neighbors
  where
    neighbors = getNeighbors pos

safe :: Set Pos -> Pos -> Bool
safe coords pos = sum (map (distance pos) coords') < 10000
  where
    coords' = Set.toList coords


-- General

distance :: Pos -> Pos -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

findDimensions :: Set Pos -> (Int, Int, Int, Int)
findDimensions coords = (minX, maxX, minY, maxY)
  where
    minX = fst $ minimumBy (comparing fst) coords
    maxX = fst $ maximumBy (comparing fst) coords
    minY = snd $ minimumBy (comparing snd) coords
    maxY = snd $ maximumBy (comparing snd) coords

getNeighbors :: Pos -> [Pos]
getNeighbors (x, y) =
  [
    (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1)]
