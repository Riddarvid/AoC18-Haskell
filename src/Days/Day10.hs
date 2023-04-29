{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Days.Day10 (solve) where
import Utils.Solution (Solver)
import Utils.Geometry (Point, point, Vector, vector, translate, findDimensions, showPoints)
import Utils.Parsing (getNegInts)

solve :: Solver
solve input = (part1, show part2)
  where
    points = parse input
    part1 = solve1 points
    part2 = solve2 points

-- Parsing

data SkyPoint = SP (Point Int) (Vector Int)

parse :: String -> [SkyPoint]
parse = map parseInput . lines

parseInput :: String -> SkyPoint
parseInput str = SP (point x y) (vector dx dy)
  where
    tokens = getNegInts str
    x = tokens !! 0
    y = tokens !! 1
    dx = tokens !! 2
    dy = tokens !! 3

-- Part 1

solve1 :: [SkyPoint] -> String
solve1 startPoints = concatMap (\conf -> showConf conf ++ "\n") mins
  where
    configs = iterate (map move) startPoints
    mins = findClosest configs

-- Part 2

solve2 :: [SkyPoint] -> Int
solve2 startPoints = length discarded
  where
    confs = iterate (map move) startPoints
    (discarded, _) = splitWhileDec confs

findClosest :: [[SkyPoint]] -> [[SkyPoint]]
findClosest confs = takeWhile ((==) minDist . distance) confs'
  where
    (_, confs') = splitWhileDec confs
    minDist = distance $ head confs'

splitWhileDec :: [[SkyPoint]] -> ([[SkyPoint]], [[SkyPoint]])
splitWhileDec [] = ([], [])
splitWhileDec [conf] = ([conf], [])
splitWhileDec xs@(conf1 : conf2 : confs)
  | distance conf1 > distance conf2 = (conf1 : taken, dropped)
  | otherwise = ([], xs)
  where
    (taken, dropped) = splitWhileDec (conf2 : confs)

move :: SkyPoint -> SkyPoint
move (SP p v) = SP (translate p v) v

distance :: [SkyPoint] -> Int
distance skyPoints = (maxX - minX) * (maxY - minY)
  where
    points = map (\(SP p _) -> p) skyPoints
    (minX, maxX, minY, maxY) = findDimensions points

showConf :: [SkyPoint] -> String
showConf skyPoints = showPoints points
  where
    points = map (\(SP p _) -> p) skyPoints