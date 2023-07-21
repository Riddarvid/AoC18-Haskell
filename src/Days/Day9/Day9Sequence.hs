{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Days.Day9.Day9Sequence (solve) where
import           Data.IntMap           (IntMap)
import qualified Data.IntMap           as IM
import           Days.Day9.Circle      (Circle)
import qualified Days.Day9.Circle      as Circle
import           Days.Day9.FocusCircle (FocusCircle)
import           AoCUtils.Days        (Solver)

solve :: Solver
solve input = (show part1, show part2)
  where
    (nPlayers, lastMarble) = parse input
    part1 = winningScore nPlayers lastMarble
    part2 = winningScore nPlayers (100 * lastMarble)

-- Parse

parse :: String -> (Int, Int)
parse input = (read $ tokens !! 0, read $ tokens !! 6)
  where
    tokens = words input

-- General

data Scores a = Scores (IntMap a) Int Int

emptyScores :: Int -> Scores a
emptyScores = Scores IM.empty 0

maxScore :: Ord a => Scores a -> a
maxScore (Scores scoreMap _ _) = maximum $ IM.elems scoreMap

addScore :: Num a => a -> Scores a -> Scores a
addScore n (Scores scoreMap current players) = Scores scoreMap' current' players
  where
    scoreMap' = IM.insertWith (+) current n scoreMap
    current' = (current `mod` players) + 1

data GameState c a = GS {
  scores  :: Scores a,
  marbles :: c a
}

startStateFocus :: (Num a) => Int -> GameState FocusCircle a
startStateFocus = startStateGeneral

startStateGeneral :: (Circle c, Num a) => Int -> GameState c a
startStateGeneral players = GS {scores = emptyScores players, marbles = Circle.singleton 0}

winningScore :: Int -> Int -> Integer
winningScore players lastMarble = maxScore $ scores endState
  where
    startState = startStateFocus players
    endState = foldl play startState [1 .. toInteger lastMarble]

play :: (Integral a, Circle c) => GameState c a -> a -> GameState c a
play gs n
  | n `mod` 23 == 0 = let
    marbles' = Circle.moveCurrent (-7) $ marbles gs
    scores' = addScore (Circle.current marbles' + n) $ scores gs
    in GS {scores = scores', marbles = Circle.delete marbles'}
  | otherwise = let
    marbles' = Circle.moveCurrent 2 $ marbles gs
    in gs{marbles = Circle.insert n marbles'}
