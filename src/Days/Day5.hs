module Days.Day5 (solve) where
import Utils.Solution (Solver)
import Data.Char (toLower, isLower, isUpper)

solve :: Solver
solve input = (show part1, show part2)
  where
    polymer = head $ lines input
    part1 = length $ reduceFully polymer
    part2 = solve2 polymer

-- General / Part 1

reduceFully :: String -> String
reduceFully polymer
  | length polymer == length polymer' = polymer
  | otherwise = reduceFully polymer'
  where
    polymer' = reduce polymer

reduce :: String -> String
reduce [] = []
reduce [c] = [c]
reduce (c1 : c2 : polymer)
  | canReact c1 c2 = reduce polymer
  | otherwise = c1 : reduce (c2 : polymer)

canReact :: Char -> Char -> Bool
canReact c1 c2 = sameUnit c1 c2 && oppositePolarity
  where
    oppositePolarity = (isLower c1 && isUpper c2) || (isUpper c1 && isLower c2)

sameUnit :: Char -> Char -> Bool
sameUnit c1 c2 = toLower c1 == toLower c2

-- Part 2

solve2 :: String -> Int
solve2 polymer = minimum $ map (length . reduceFully . removeUnit polymer) ['a' .. 'z']

removeUnit :: String -> Char -> String
removeUnit polymer unit = 
  foldr (\c polymer' -> if sameUnit c unit then polymer' else c : polymer') "" polymer
