{-# LANGUAGE TupleSections #-}

module Day7 (solve) where
import Solution (Solver)
import Data.Set (Set, (\\))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, delete)
import Control.Monad.State (execState, State, unless, gets, modify)
import Data.Char (ord)

solve :: Solver
solve input = (part1, show part2)
  where
    (allTasks, reqs) = parseReqs input
    part1 = solve1 allTasks reqs
    part2 = solve2 allTasks reqs

-- Data types

data Entry = Entry Char Char

type Prerequisites = Map Char (Set Char)

-- Parsing

parseReqs :: String -> ([Char], Prerequisites)
parseReqs input = (sort allTasks, catEntries allTasks entries)
  where
    entries = map parseEntry $ lines input
    allTasks = findAllTasks entries

parseEntry :: String -> Entry
parseEntry str = Entry (head $ tokens !! 1) (head $ tokens !! 7)
  where
    tokens = words str

catEntries :: [Char] -> [Entry] -> Prerequisites
catEntries tasks = foldr (\(Entry pre post) -> Map.adjust (Set.insert pre) post) startMap
  where
    startMap = Map.fromList $ map (, Set.empty) tasks

findAllTasks :: [Entry] -> [Char]
findAllTasks entries = Set.toList $ Set.union pres posts
  where
    pres = foldr (\(Entry c _) pres' -> Set.insert c pres') Set.empty entries
    posts = foldr (\(Entry _ c) pres' -> Set.insert c pres') Set.empty entries


-- Part 1

-- Remaining input and produced output
type TaskState1 = ([Char], [Char])

solve1 :: [Char] -> Prerequisites -> [Char]
solve1 tasks reqs = reverse $ snd $ until (null . fst) (doNext reqs) (tasks, [])

doNext :: Prerequisites -> TaskState1 -> TaskState1
doNext reqs (remaining, done) = (remaining', done')
  where
    task = findNextTask reqs done remaining
    remaining' = delete task remaining
    done' = task : done

findNextTask :: Foldable t0 => Prerequisites -> t0 Char -> [Char] -> Char
findNextTask reqs done = head . findNextTasks reqs done

findNextTasks :: Foldable t0 => Prerequisites -> t0 Char -> [Char] -> [Char]
findNextTasks reqs done = filter (isReady reqs done)

isReady :: Foldable t => Prerequisites -> t Char -> Char -> Bool
isReady reqs done task = all (`elem` done) pres
  where
    pres = reqs ! task

-- Part 2

data TaskState2 = TS2 {
  tsReqs :: Prerequisites,
  tsRemaining :: [Char],
  tsDone :: Set Char,
  tsCurrent :: Set (Char, Int),
  tsTime :: Int
}

solve2 :: [Char] -> Prerequisites -> Int
solve2 tasks reqs = tsTime $ execState untilDone startState
  where
    startState = TS2 {
      tsReqs = reqs,
      tsRemaining = tasks,
      tsDone = Set.empty,
      tsCurrent = Set.empty,
      tsTime = 0}

untilDone :: State TaskState2 ()
untilDone = do
  tick
  done <- isDone
  unless done untilDone

-- 1: assign ready tasks if workers available
-- 2: increase time counter, reduce current task times
-- 3: remove finished tasks
tick :: State TaskState2 ()
tick = do
  assignReady
  handleTime
  removeFinished

-- Assign new tasks

assignReady :: State TaskState2 ()
assignReady = do
  current <- gets tsCurrent
  if Set.size current == 5
    then return ()
    else do
      let free = 5 - Set.size current
      reqs <- gets tsReqs
      done <- gets tsDone
      remaining <- gets tsRemaining
      let newTasks = take free $ findNextTasks reqs done remaining
      let newTasks' = map addCost newTasks
      modify (\s -> s{tsCurrent = Set.union current (Set.fromList newTasks')})
      modify (\s -> s{tsRemaining = filter (`notElem` newTasks) remaining})

addCost :: Char -> (Char, Int)
addCost c = (c, cost c)

cost :: Char -> Int
cost c = 60 + (ord c - ord 'A' + 1)

-- Handle time

handleTime :: State TaskState2 ()
handleTime = do
  modify (\s -> s{tsCurrent = Set.map (\(c, time) -> (c, time - 1)) $ tsCurrent s})
  modify (\s -> s{tsTime = tsTime s + 1})

-- Remove finished tasks

removeFinished :: State TaskState2 ()
removeFinished = do
  current <- gets tsCurrent
  let newDone = Set.filter (\(_, time) -> time == 0) current
  modify (\s -> s{tsCurrent = current \\ newDone})
  let newDone' = Set.map fst newDone
  modify (\s -> s{tsDone = Set.union (tsDone s) newDone'})

isDone :: State TaskState2 Bool
isDone = do
  remaining <- gets tsRemaining
  current <- gets tsCurrent
  return $ null remaining && null current
