module Days.Day8 (solve) where
import AoCUtils.Days (Solver)
import Control.Monad.State (StateT, evalStateT, MonadState (get, put))
import Control.Monad.Error.Class (MonadError (throwError))

solve :: Solver
solve input = (show part1, show part2)
  where
    tree = parseTree input
    part1 = metaSum tree
    part2 = value tree

data Tree = Tree [Tree] [Int] deriving Show

type Error = Either String

-- Parsing

parseTree :: String -> Tree
parseTree input = case evalStateT parseTree' startState of
  Left err -> error $ show err
  Right tree -> tree
  where
    startState = map read $ words input

parseTree' :: StateT [Int] Error Tree
parseTree' = do
  nChildren <- next
  nMeta <- next
  children <- doTimes nChildren parseTree'
  meta <- doTimes nMeta next
  return (Tree children meta)

next :: StateT [a] Error a
next = do
  input <- get
  case input of
    [] -> throwError "All input consumed"
    (x : xs) -> do
      put xs
      return x

doTimes :: Monad m => Int -> m a -> m [a]
doTimes 0 _ = return []
doTimes n action = do
  res <- action
  ress <- doTimes (n - 1) action
  return (res : ress)

-- Part 1

metaSum :: Tree -> Int
metaSum (Tree children meta) = sum $ map metaSum children ++ meta

-- Part 2

value :: Tree -> Int
value tree@(Tree [] _) = metaSum tree
value (Tree children meta) = sum $ map value chosen
  where
    chosen = foldr (refChild children) [] meta

refChild :: [Tree] -> Int -> [Tree] -> [Tree]
refChild allChildren ref chosen
  | ref > length allChildren = chosen
  | otherwise = (allChildren !! (ref - 1)) : chosen