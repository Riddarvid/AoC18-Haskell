module Days.Day1 (solve) where

import AoCUtils.Days (Solver)
import Data.Set (Set)
import qualified Data.Set as Set

solve :: Solver
solve input = (show part1, show part2)
  where
    frequencies = parse input
    part1 = solve1 frequencies
    part2 = solve2 frequencies

parse :: String -> [Int]
parse = map parseLine . lines

parseLine :: String -> Int
parseLine ('+' : str) = read str
parseLine ('-' : str) = - read str
parseLine _ = error "Invalid input"

solve1 :: [Int] -> Int
solve1 = sum

solve2 :: [Int] -> Int
solve2 frequencies = firstTwice (cycle frequencies) Set.empty 0

firstTwice :: [Int] -> Set Int -> Int -> Int
firstTwice (next : freqs) encountered current 
  | Set.member current encountered = current
  | otherwise = firstTwice freqs (Set.insert current encountered) current'
  where
    current' = next + current
firstTwice _ _ _ = error "List should be infinite"
