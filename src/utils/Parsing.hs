module Parsing (getInts, getIntegers) where

import Text.Regex.PCRE (AllTextMatches (getAllTextMatches), (=~))

intRegex :: String
intRegex = "\\d+"


getInts :: String -> [Int]
getInts input = map read $ getAllTextMatches $ input =~ intRegex

getIntegers :: String -> [Integer]
getIntegers input = map read $ getAllTextMatches $ input =~ intRegex