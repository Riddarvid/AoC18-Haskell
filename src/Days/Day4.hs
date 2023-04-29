{-# LANGUAGE TupleSections, InstanceSigs #-}

module Days.Day4 (solve) where
import Utils.Solution (Solver)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Utils.Parsing (getInts)
import Data.List (maximumBy, sortOn)
import Data.Ord (comparing)

solve :: Solver
solve input = (show part1, show part2)
  where
    guards = parseGuards input
    part1 = solve1 guards
    part2 = solve2 guards

-- Part 1

solve1 :: [Guard] -> Int
solve1 guards = id' * sleepMinute
  where
    mostAsleep@(Guard id' _) = findMostAsleep guards
    sleepMinute = fst $ mostAsleepMinute mostAsleep

mostAsleepMinute :: Guard -> (Int, Int)
mostAsleepMinute (Guard _ shifts) = maximumBy (comparing snd) $ Map.toList sleepMap
  where
    startMap = Map.fromList $ map (, 0) [0 .. 59]
    sleepMap = foldr sumMinutes startMap shifts

sumMinutes :: Shift -> Map Int Int -> Map Int Int
sumMinutes (Shift _ sleepMins) minMap = foldr (Map.adjust (+1)) minMap sleepMins

findMostAsleep :: [Guard] -> Guard
findMostAsleep = maximumBy $ comparing minutesAsleep

minutesAsleep :: Guard -> Int
minutesAsleep (Guard _ shifts) = sum $ map minutesAsleepShift shifts

minutesAsleepShift :: Shift -> Int
minutesAsleepShift (Shift _ sleepMins) = Set.size sleepMins

-- Part 2

-- Idea: For each guard, find their most frequently slept minute. Compare them and take the max.
solve2 :: [Guard] -> Int
solve2 guards = id' * minute
  where
    mostFrequents = map getMostFrequent guards
    (Guard id' _, minute, _) = maximumBy (comparing (\(_, _, freq) -> freq)) mostFrequents

getMostFrequent :: Guard -> (Guard, Int, Int)
getMostFrequent guard = (guard, minute, freq)
  where
    (minute, freq) = mostAsleepMinute guard

-- Parsing

parseGuards :: String -> [Guard]
parseGuards input = aggregateGuards shifts
  where
    entries = map parseEntry $ lines input
    sortedEntries = sortOn timeToList entries
    shifts = parseShifts sortedEntries

aggregateGuards :: [Shift] -> [Guard]
aggregateGuards = mapToGuards . foldr addShift Map.empty

addShift :: Shift -> Map Int [Shift] -> Map Int [Shift]
addShift shift@(Shift id' _) guardMap = case Map.lookup id' guardMap of
  Just shifts -> Map.insert id' (shift : shifts) guardMap
  Nothing -> Map.insert id' [shift] guardMap

mapToGuards :: Map Int [Shift] -> [Guard]
mapToGuards = map (uncurry Guard) . Map.toList

parseShifts :: [Entry] -> [Shift]
parseShifts [] = []
parseShifts entries = shift : parseShifts entries'
  where
    (shift, entries') = parseShift entries

parseShift :: [Entry] -> (Shift, [Entry])
parseShift (Entry _ _ (Start id') : entries) = (Shift id' (getAsleep entries'), entries'')
  where
    (entries', entries'') = span isWakeSleep entries
parseShift _ = error "First entry should be start shift."

getAsleep :: [Entry] -> Set Int
getAsleep [] = Set.empty
getAsleep (Entry _ (Time _ sleepMin) _ : Entry _ (Time _ wakeMin) _ : entries)
  = Set.fromList [sleepMin .. wakeMin - 1] `Set.union` getAsleep entries
getAsleep _ = error "Asleep should always have a matching wake"

isWakeSleep :: Entry -> Bool
isWakeSleep (Entry _ _ (Start _)) = False
isWakeSleep _ = True

timeToList :: Entry -> [Int]
timeToList (Entry (Date year month day) (Time hour minute) _) = [year, month, day, hour, minute]

parseEntry :: String -> Entry
parseEntry str
  | length tokens == 5 = Entry date time action
  | otherwise = Entry date time (Start id')
  where
    tokens = getInts str
    date = Date (head tokens) (tokens !! 1) (tokens !! 2)
    time = Time (tokens !! 3) (tokens !! 4)
    id' = tokens !! 5
    action = if words str !! 2 == "wakes"
      then Wake
      else Sleep

-- Data types

data Date = Date Int Int Int

data Time = Time Int Int

data Entry = Entry Date Time Action

data Action = Start Int | Wake | Sleep

data Shift = Shift Int (Set Int)

data Guard = Guard Int [Shift]

-- Pretty printing

instance Show Entry where
  show :: Entry -> String
  show (Entry date time action) = show date ++ " " ++ show time ++ " " ++ show action

instance Show Time where
  show :: Time -> String
  show (Time hour minute) = hPad ++ show hour ++ ":" ++ mPad ++ show minute
    where
      hPad = if hour < 10 then "0" else ""
      mPad = if minute < 10 then "0" else ""

instance Show Date where
  show :: Date -> String
  show (Date year month day) = show year ++ "-" ++ show month ++ "-" ++ show day

instance Show Action where
  show :: Action -> String
  show (Start id') = "Guard #" ++ show id' ++ " begins shift"
  show Wake = "wakes up"
  show Sleep = "falls asleep"

instance Show Shift where
  show :: Shift -> String
  show (Shift id' sleepMins) = "Guard #" ++ show id' ++ " slept during " ++ show sleepMins

instance Show Guard where
  show :: Guard -> String
  show (Guard id' shifts) = "Guard #" ++ show id' ++ "\n" ++ unlines (map show shifts)
