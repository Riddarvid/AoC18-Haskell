{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Day9alt (solve) where
import Solution (Solver)
import Data.Map (Map)
import qualified Data.Map as Map

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

data GameState = GS {
  gsPlayers :: Map Int Integer,
  gsNextPlayer :: Int,
  gsMarbles :: [Int]
}

winningScore :: Int -> Int -> Integer
winningScore nPlayers lastMarble = maximum $ Map.elems $ gsPlayers endState
  where
    endState = foldl placeMarble startState [1 .. lastMarble]
    startState = GS {gsPlayers = startPlayers, gsNextPlayer = 1, gsMarbles = [0]}
    startPlayers = Map.fromList $ zip [1 .. nPlayers] [0 ..]

-- Assumption: The current marble is always the first element of the list
placeMarble :: GameState -> Int -> GameState
placeMarble s marble = advanceTurn s'
  where
    s' 
      | marble `mod` 23 == 0 = deleteAt s (-7) marble
      | otherwise = insertAt s 1 marble

deleteAt :: GameState -> Int -> Int -> GameState
deleteAt s i marble = score s{gsMarbles = marbles'} (toInteger (marble + removed))
  where
    marbles = gsMarbles s
    i' = marble + i -- TODO maybe +/- 1?
    (start, end) = splitAt i' marbles
    marbles' = tail (end ++ start)
    removed = head end

insertAt :: GameState -> Int -> Int -> GameState
insertAt s i marble = s{gsMarbles = marbles'}
  where
    marbles = gsMarbles s
    (start, end) = splitAt i marbles
    marbles' = marble : end ++ start


advanceTurn :: GameState -> GameState
advanceTurn s = s{gsNextPlayer = nextPlayer}
  where
    nextPlayer = if gsNextPlayer s < Map.size (gsPlayers s)
      then gsNextPlayer s + 1
      else 1

score :: GameState -> Integer -> GameState
score s n = s{gsPlayers = Map.adjust (+ n) (gsNextPlayer s) (gsPlayers s)}