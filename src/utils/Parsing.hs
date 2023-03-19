module Parsing (getInts, getIntegers, getNegInts) where

import Text.Regex.PCRE (AllTextMatches (getAllTextMatches), (=~))

negIntRegex :: String
negIntRegex = "-?\\d+"

intRegex :: String
intRegex = "\\d+"

readInt :: String -> Int
readInt = read

readInteger :: String -> Integer
readInteger = read

getInts :: String -> [Int]
getInts = getAll readInt intRegex

getNegInts :: String -> [Int]
getNegInts = getAll readInt negIntRegex

getIntegers :: String -> [Integer]
getIntegers = getAll readInteger intRegex

getAll :: (String -> a) -> String -> String -> [a]
getAll readF regex input = map readF $ allMatches regex input

allMatches :: String -> String -> [String]
allMatches regex input = getAllTextMatches $ input =~ regex