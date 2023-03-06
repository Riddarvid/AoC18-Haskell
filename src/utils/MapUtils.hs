module MapUtils (incOrInsert) where

import Data.Map (Map)
import qualified Data.Map as Map

incOrInsert :: Ord a => a -> Map a Int -> Map a Int
incOrInsert c letterCount
  | Map.member c letterCount = Map.adjust (+ 1) c letterCount
  | otherwise = Map.insert c 1 letterCount