module Main (main) where

import Solution (Solution)
import Day1 (solve)
import Day2 (solve)
import Day3 (solve)
import Day4 (solve)
import Day5 (solve)
import Day6 (solve)
import Day7 (solve)
import Day8 (solve)

day :: Int
day = 8

main :: IO ()
main = do
  input <- readFile ("input/input" ++ show day ++ ".txt")
  let solution = (solvers !! (day - 1)) input
  printSolution solution

solvers :: [String -> Solution]
solvers = [
  Day1.solve, Day2.solve, Day3.solve, Day4.solve, Day5.solve, Day6.solve, 
  Day7.solve, Day8.solve]

printSolution :: Solution -> IO ()
printSolution (part1, part2) = do
  putStrLn "Part1:"
  putStrLn part1
  putStrLn ""
  putStrLn "Part2:"
  putStrLn part2