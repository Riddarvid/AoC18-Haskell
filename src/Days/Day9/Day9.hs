{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Days.Day9.Day9 (solve) where
import           AoCUtils.Days         (Solver)
import           Control.Monad.State   (State, execState, gets, modify)
import           Data.Foldable         (traverse_)
import           Data.IntMap           (IntMap)
import qualified Data.IntMap           as IM
import           Days.Day9.Circle      (Circle)
import qualified Days.Day9.Circle      as C
import           Days.Day9.FocusCircle (FocusCircle)

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

data GameState c = GS {
  gsScoreMap      :: IntMap Integer,
  gsCurrentPlayer :: Int,
  gsNPlayers      :: Int,
  gsMarbleCircle  :: c Integer
}

mkGS :: Int -> GameState FocusCircle
mkGS nPlayers = GS {
  gsScoreMap = IM.empty,
  gsCurrentPlayer = 0,
  gsNPlayers = nPlayers,
  gsMarbleCircle = C.singleton 0
}

maxScore :: GameState c -> Integer
maxScore = maximum . gsScoreMap

winningScore :: Int -> Int -> Integer
winningScore nPlayers lastMarble = maxScore endState
  where
    startState = mkGS nPlayers
    endState = execState (traverse_ play [1 .. toInteger lastMarble]) startState

play :: Circle c => Integer -> State (GameState c) ()
play marble = do
  if marble `mod` 23 == 0
    then do
      moveFocus (-7)
      removed <- remove
      addScore (marble + removed)
    else do
      moveFocus 1
      insert marble
  incCurrentPlayer

insert :: Circle c => Integer -> State (GameState c) ()
insert marble = modify (\s -> s{gsMarbleCircle = C.insert marble $ gsMarbleCircle s})

remove :: Circle c => State (GameState c) Integer
remove = do
  circle <- gets gsMarbleCircle
  let (marble, circle') = C.remove circle
  modify (\s -> s{gsMarbleCircle = circle'})
  return marble

-- Adds score and incs current player
addScore :: Integer -> State (GameState c) ()
addScore n = do
  current <- gets gsCurrentPlayer
  modify (\s -> s{gsScoreMap = IM.insertWith (+) current n (gsScoreMap s)})

incCurrentPlayer :: State (GameState c) ()
incCurrentPlayer = modify (\s -> s{gsCurrentPlayer = (gsCurrentPlayer s `mod` gsNPlayers s) + 1})

moveFocus :: Circle c => Int -> State (GameState c) ()
moveFocus n = modify (\s -> s{gsMarbleCircle = C.moveFocus n $ gsMarbleCircle s})
