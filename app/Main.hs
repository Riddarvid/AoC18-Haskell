module Main (main) where

import Utils.Solution (Solution)
import Days.Day1 as Day1
import Days.Day2 as Day2
import Days.Day3 as Day3
import Days.Day4 as Day4
import Days.Day5 as Day5
import Days.Day6 as Day6
import Days.Day7 as Day7
import Days.Day8 as Day8
import Days.Day9.Day9Sequence as Day9
import Days.Day10 as Day10
import Days.Day11 as Day11

day :: Int
day = 9

main :: IO ()
main = do
  input <- readFile ("input/input" ++ show day ++ ".txt")
  let solution = (solvers !! (day - 1)) input
  printSolution solution

solvers :: [String -> Solution]
solvers = [
  Day1.solve, Day2.solve, Day3.solve, Day4.solve, Day5.solve, Day6.solve, 
  Day7.solve, Day8.solve, Day9.solve, Day10.solve, Day11.solve]

printSolution :: Solution -> IO ()
printSolution (part1, part2) = do
  putStrLn "Part1:"
  putStrLn part1
  putStrLn ""
  putStrLn "Part2:"
  putStrLn part2