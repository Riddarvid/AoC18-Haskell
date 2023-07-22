module Utils.Days (solvers) where

import           AoCUtils.Days  (Solver)
import           Days.Day1      as Day1 (solve)
import           Days.Day10     as Day10 (solve)
--import           Days.Day11             as Day11 (solve)
import           Days.Day2      as Day2 (solve)
import           Days.Day3      as Day3 (solve)
import           Days.Day4      as Day4 (solve)
import           Days.Day5      as Day5 (solve)
import           Days.Day6      as Day6 (solve)
import           Days.Day7      as Day7 (solve)
import           Days.Day8      as Day8 (solve)
import           Days.Day9.Day9 as Day9 (solve)

solvers :: [Solver]
solvers = [
  Day1.solve, Day2.solve, Day3.solve, Day4.solve, Day5.solve, Day6.solve,
  Day7.solve, Day8.solve, Day9.solve, Day10.solve]
