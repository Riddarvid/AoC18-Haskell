{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Day10 (solve) where
import Solution (Solver)
import Geometry (Point, point, Vector, vector, translate, findDimensions, showPoints)
import Parsing (getNegInts)

solve :: Solver
solve input = (part1, "")
  where
    points = parse input
    part1 = solve1 points

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

findClosest :: [[SkyPoint]] -> [[SkyPoint]]
findClosest confs = takeWhile ((==) minDist . distance) confs'
  where
    confs' = dropWhileDec confs
    minDist = distance $ head confs'

dropWhileDec :: [[SkyPoint]] -> [[SkyPoint]]
dropWhileDec [] = []
dropWhileDec [conf] = [conf]
dropWhileDec xs@(conf1 : conf2 : confs)
  | distance conf1 > distance conf2 = dropWhileDec (conf2 : confs)
  | otherwise = xs

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