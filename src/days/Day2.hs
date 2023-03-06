module Day2 (solve) where
import Solution (Solution)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (find)

solve :: String -> Solution
solve input = (show part1, part2)
  where
    ids = lines input
    part1 = checksum ids
    part2 = solvePart2 ids

-- Part 1

checksum :: [String] -> Integer
checksum ids = toInteger twos * toInteger threes
  where
    twos = length $ filter (containsN 2) ids
    threes = length $ filter (containsN 3) ids

containsN :: Int -> String -> Bool
containsN n str = not . Map.null $ Map.filter (== n) letterCount
  where
    letterCount = countLetters str

countLetters :: String -> Map Char Int
countLetters = foldr incOrInsert Map.empty

incOrInsert :: Char -> Map Char Int -> Map Char Int
incOrInsert c letterCount
  | Map.member c letterCount = Map.adjust (+ 1) c letterCount
  | otherwise = Map.insert c 1 letterCount

-- Part2

solvePart2 :: [String] -> String
solvePart2 ids = sameLetters matchingIds
  where
    matchingIds = findMatching ids

findMatching :: [String] -> (String, String)
findMatching (id' : ids) = case findSimilar id' ids of
  Nothing -> findMatching ids
  Just id'' -> (id', id'')
findMatching _ = error "No similar ids found."

findSimilar :: String -> [String] -> Maybe String
findSimilar id' = find (isSimilar id')

isSimilar :: String -> String -> Bool
isSimilar id1 id2 = length different == 1
  where
    different = filter (uncurry (/=)) $ zip id1 id2

sameLetters :: (String, String) -> String
sameLetters (id1, id2) = map fst (filter (uncurry (==)) $ zip id1 id2)
