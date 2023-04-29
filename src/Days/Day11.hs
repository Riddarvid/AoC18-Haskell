module Days.Day11 (solve, powerLevel) where
import Utils.Solution (Solver)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Utils.Geometry (Point (pX, pY), point, vector, translate)
import Data.Foldable (maximumBy)
import Data.Ord (comparing)

solve :: Solver
solve input = (show part1, show part2)
  where
    sId = read $ head $ lines input
    powerLevels = findPowerLevels sId
    part1 = bestRegion powerLevels 3
    part2 = 
      maximum $ map (totalPower powerLevels . bestRegion powerLevels) [1 .. size]

size :: Int
size = 300

type PowerMap = Map (Point Int) Int

findPowerLevels :: Int -> PowerMap
findPowerLevels sId = Map.fromList 
  [let p = point x y in (p, powerLevel sId p) | x <- [1 .. size], y <- [1 .. size]]

powerLevel :: Int -> Point Int -> Int
powerLevel sId p = level''''
  where
    rackID = pX p + 10
    level = rackID * pY p
    level' = level + sId
    level'' = level' * rackID
    level''' = (level'' `div` 100) `mod` 10
    level'''' = level''' - 5
 
data Region = RG (Point Int) Int
  deriving Show

bestRegion :: PowerMap -> Int -> Region
bestRegion powers side = maximumBy (comparing (totalPower powers)) regions
  where
    regions = [RG (point x y) side | x <- [1 .. size - side], y <- [1 .. size - side]]

totalPower :: PowerMap -> Region -> Int
totalPower powers (RG p side) = sum $ map (powers !) points
  where
    vectors = [vector x y | x <- [0 .. side - 1], y <- [0 .. side - 1]]
    points = map (translate p) vectors
