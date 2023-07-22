module Days.Day11 (solve, powerLevel) where
import           AoCUtils.Days     (Solver)
import           AoCUtils.Geometry (Point (moveBy), Point2 (P2, p2X, p2Y))
import           Data.Foldable     (maximumBy)
import           Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as HM
import           Data.Ord          (comparing)

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

type PowerMap = HashMap (Point2 Int) Int

findPowerLevels :: Int -> PowerMap
findPowerLevels sId = HM.fromList
  [let p = P2 x y in (p, powerLevel sId p) | x <- [1 .. size], y <- [1 .. size]]

powerLevel :: Int -> Point2 Int -> Int
powerLevel sId p = level''''
  where
    rackID = p2X p + 10
    level = rackID * p2Y p
    level' = level + sId
    level'' = level' * rackID
    level''' = (level'' `div` 100) `mod` 10
    level'''' = level''' - 5

data Region = RG (Point2 Int) Int
  deriving Show

bestRegion :: PowerMap -> Int -> Region
bestRegion powers side = maximumBy (comparing (totalPower powers)) regions
  where
    regions = [RG (P2 x y) side | x <- [1 .. size - side], y <- [1 .. size - side]]

totalPower :: PowerMap -> Region -> Int
totalPower powers (RG p side) = sum $ map (powers !) points
  where
    vectors = [P2 x y | x <- [0 .. side - 1], y <- [0 .. side - 1]]
    points = map (moveBy p) vectors
