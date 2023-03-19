{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Day9 (solve) where
import Solution (Solver)
import Control.Monad.State (State, execState, gets, modify, MonadState (get))

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as Map
import Debug.Trace (trace)

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

-- TODO: Look up how to solve this in a better way

-- The current marble is always
type MarbleMap = HashMap Int (Int, Int)

data GameState = GS {
  gsPlayers :: HashMap Int Integer,
  gsNextPlayer :: Int,
  gsNextMarble :: Int,
  gsLastMarble :: Int,
  gsMarbles :: MarbleMap,
  gsCurrentMarble :: Int
} deriving Show

makeGameState :: Int -> Int -> GameState
makeGameState nPlayers lastMarble = GS {
      gsPlayers = Map.fromList $ map (, 0) [1 .. nPlayers],
      gsNextPlayer = 1,
      gsNextMarble = 1,
      gsLastMarble = lastMarble,
      gsMarbles = Map.singleton 0 (0, 0),
      gsCurrentMarble = 0
      }

winningScore :: Int -> Int -> Integer
winningScore nPlayers lastMarble = maximum $ Map.elems $ gsPlayers gs
  where
    gs = execState playGame startState
    startState = makeGameState nPlayers lastMarble

-- 1. if done return
-- 2. place marble + maybe scoring
-- 3. advance turn
playGame :: State GameState ()
playGame = do
  done <- isDone
  if done
    then return ()
    else do
      placeMarble
      advanceTurn
      playGame

showMarbles :: GameState -> String
showMarbles s = show marbles
  where
    marbles = gsMarbles s

marblesToList :: MarbleMap -> [Int]
marblesToList marbles = 0 : takeWhile (/= 0) (tail marbleList)
  where
    marbleList = iterate (snd . (marbles !)) 0

isDone :: State GameState Bool
isDone = do
  nextMarble <- gets gsNextMarble
  lastMarble <- gets gsLastMarble
  return $ nextMarble == lastMarble + 1

-- Marble placing

placeMarble :: State GameState ()
placeMarble = do
  marble <- getNextMarble
  if marble `mod` 23 /= 0
    then insertAt 1 marble
    else do
      score marble
      removed <- removeAt (-7)
      score removed

getNextMarble :: State GameState Int
getNextMarble = do
  nextMarble <- gets gsNextMarble
  modify (\s -> s{gsNextMarble = nextMarble + 1})
  return nextMarble

insertAt :: Int -> Int -> State GameState ()
insertAt n toInsert = do
  marble <- marbleAt n
  marbles <- gets gsMarbles
  let marbles' = insertMarble marble toInsert marbles
  modify (\s -> s{gsMarbles = marbles', gsCurrentMarble = toInsert})

-- Special case when we only have one marble in the circle: left == right
insertMarble :: Int -> Int -> MarbleMap -> MarbleMap
insertMarble other toInsert marbles
  | Map.size marbles == 1 =
    Map.union (Map.fromList [(toInsert, (other, other)), (other, (toInsert, toInsert))])
    marbles
insertMarble left toInsert marbles = Map.union marbles' marbles
  where
    (ll, lr) = marbles ! left
    (_, rr) = marbles ! lr
    marbles' = 
      Map.fromList [(toInsert, (left, lr)), (lr, (toInsert, rr)), (left, (ll, toInsert))]

removeAt :: Int -> State GameState Int
removeAt n = do
  marble <- marbleAt n
  marbles <- gets gsMarbles
  let current' = snd $ marbles ! marble
  let marbles' = removeMarble marble marbles
  modify (\s -> s{gsMarbles = marbles', gsCurrentMarble = current'})
  return marble

removeMarble :: Int -> MarbleMap -> MarbleMap
removeMarble toRemove marbles =
  Map.delete toRemove $
  Map.insert right (left, rr) $
  Map.insert left (ll, right) marbles
  where
    (left, right) = marbles ! toRemove
    (ll, _) = marbles ! left
    (_, rr) = marbles ! right

marbleAt :: Int -> State GameState Int
marbleAt 0 = gets gsCurrentMarble
marbleAt n = do
  marbles <- gets gsMarbles
  current <- gets gsCurrentMarble
  let current' = f $ marbles ! current
  modify (\s -> s{gsCurrentMarble = current'})
  marbleAt (n - m)
  where
    f = if n > 0 then snd else fst
    m = if n > 0 then 1 else (-1)

score :: Int -> State GameState ()
score marble = do
  player <- gets gsNextPlayer
  players <- gets gsPlayers
  let oldScore = players ! player
  let newScore = oldScore + toInteger marble
  modify (\s -> s{gsPlayers = Map.insert player newScore players})

-- Advancing turn

advanceTurn :: State GameState ()
advanceTurn = do
  nextPlayer <- gets gsNextPlayer
  n <- gets (Map.size . gsPlayers)
  let nextPlayer' = if nextPlayer < n then nextPlayer + 1 else 1
  modify (\s -> s{gsNextPlayer = nextPlayer'})