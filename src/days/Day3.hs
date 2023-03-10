module Day3 (solve) where
import Solution (Solver)
import Parsing (getInts)

import Data.Map (Map, (!))
import qualified Data.Map as Map
import MapUtils (incOrInsert)
import Data.Maybe (fromJust)
import Data.Foldable (find)

type Pos = (Int, Int)

data Claim = Claim {
  cId :: Int,
  cPos :: Pos,
  cArea :: (Int, Int)
} deriving Show

solve :: Solver
solve input = (show part1, show part2)
  where
    claims = parseInput input
    part1 = getOverlaps claims
    part2 = findNoOverlap claims

-- Parsing

parseInput :: String -> [Claim]
parseInput = map parseClaim . lines

parseClaim :: String -> Claim
parseClaim str = Claim {cId = id', cPos = pos, cArea = area}
  where
    tokens = getInts str
    id' = head tokens
    pos = (tokens !! 1, tokens !! 2)
    area = (tokens !! 3, tokens !! 4)

-- Part 1

getOverlaps :: [Claim] -> Int
getOverlaps = Map.size . Map.filter (> 1) . claimMap

claimMap :: [Claim] -> Map Pos Int
claimMap = foldr (\claim posMap -> foldr incOrInsert posMap (positions claim)) Map.empty

positions :: Claim -> [Pos]
positions claim = [(x, y) | x <- [posX .. posX + width - 1], y <- [posY .. posY + height - 1]]
  where
    (posX, posY) = cPos claim
    (width, height) = cArea claim

-- Part2

findNoOverlap :: [Claim] -> Int
findNoOverlap claims = cId $ fromJust $ find (noOverlap posMap) claims
  where
    posMap = claimMap claims

noOverlap :: Map Pos Int -> Claim -> Bool
noOverlap posMap claim = all (\pos -> posMap ! pos == 1) $ positions claim


{- Fancy parsing
parseClaim :: String -> Claim
parseClaim str = case parse claimParser "" str of
  Left err -> error $ show err
  Right claim -> claim

claimParser :: Parsec String () Claim
claimParser = do
  _ <- char '#'
  id' <- numberParser
  _ <- space
  _ <- char '@'
  _ <- space
  pos <- tupleParser
  _ <- char ':'
  _ <- space
  area <- tupleParser
  return $ Claim {cId = id', cPos= pos, cArea = area}

tupleParser :: Parsec String () (Int, Int)
tupleParser = do
  n1 <- numberParser
  _ <- anyChar
  n2 <- numberParser
  return (n1, n2)

numberParser :: Parsec String () Int
numberParser = read <$> many digit
-}