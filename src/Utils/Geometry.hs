{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Utils.Geometry (
  Point,
  point,
  pX,
  pY,
  Vector,
  vector,
  vX,
  vY,
  translate,
  findDimensions,
  showPoints
  ) where
import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy, toList)

import Data.Set (Set)
import qualified Data.Set as Set

data Point a = (Num a) => P {
  pX :: a,
  pY :: a
}

deriving instance Eq a => Eq (Point a)
deriving instance Ord a => Ord (Point a)

instance (Show a) => Show (Point a) where
  show :: Point a -> String
  show p = "(" ++ show (pX p) ++ ", " ++ show (pY p) ++ ")"

point :: Num a => a -> a -> Point a
point = P

data Vector a = (Num a) => V {
  vX :: a,
  vY :: a
}

vector :: Num a => a -> a -> Vector a
vector = V 

translate :: Point a -> Vector a -> Point a
translate (P x y) (V dx dy) = P (x + dx) (y + dy)


findDimensions :: (Ord a, Foldable t) => t (Point a) -> (a, a, a, a)
findDimensions coords = (minX, maxX, minY, maxY)
  where
    minX = pX $ minimumBy (comparing pX) coords
    maxX = pX $ maximumBy (comparing pX) coords
    minY = pY $ minimumBy (comparing pY) coords
    maxY = pY $ maximumBy (comparing pY) coords

showPoints :: (Foldable t, Integral a) => t (Point a) -> String
showPoints points = unlines $ map (showRow points' minX maxX) [minY .. maxY]
  where
    (minX, maxX, minY, maxY) = findDimensions points
    points' = Set.fromList $ toList points

showRow :: (Integral a) => Set (Point a) -> a -> a -> a -> String
showRow points minX maxX y = 
  [if Set.member (point x y) points then '#' else '.' | x <- [minX .. maxX]]