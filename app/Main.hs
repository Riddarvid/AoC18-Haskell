module Main (main) where

import Solution (Solution)
import Day1 (solve)
import Day2 (solve)

day :: Int
day = 2

main :: IO ()
main = do
  input <- readFile ("input/input" ++ show day ++ ".txt")
  let solution = (solvers !! (day - 1)) input
  printSolution solution

solvers :: [String -> Solution]
solvers = [Day1.solve, Day2.solve]

printSolution :: Solution -> IO ()
printSolution (part1, part2) = do
  putStrLn "Part1:"
  putStrLn part1
  putStrLn ""
  putStrLn "Part2:"
  putStrLn part2