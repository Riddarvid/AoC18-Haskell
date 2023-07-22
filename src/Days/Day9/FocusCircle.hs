{-# LANGUAGE InstanceSigs #-}
module Days.Day9.FocusCircle (FocusCircle) where
import           Days.Day9.Circle (Circle (..))

data FocusCircle a = FC [a] a [a]

instance Circle FocusCircle where
  singleton :: a -> FocusCircle a
  singleton f = FC [] f []
  focus :: FocusCircle a -> a
  focus (FC _ f _) = f
  moveFocus :: Int -> FocusCircle a -> FocusCircle a
  moveFocus n c = iterate move c !! abs n
    where
      move = if n < 0 then moveLeft else moveRight
  insert :: a -> FocusCircle a -> FocusCircle a -- Insert element to the right of focus
  insert f' (FC ls f rs) = FC (f :ls) f' rs
  remove :: FocusCircle a -> (a, FocusCircle a)
  remove (FC ls f (r : rs)) = (f, FC ls r rs) -- Element to the right of focus becomes new focus
  remove c@(FC _ _ [])      = remove $ balanceRight c

balanceRight :: FocusCircle a -> FocusCircle a
balanceRight (FC ls f rs) = FC lsBuffert f (rs ++ reverse ls')
  where
    (lsBuffert, ls') = if longerThan 10 ls
      then splitAt 10 ls
      else ([], ls)

longerThan :: Int -> [a] -> Bool
longerThan _ []       = False
longerThan 0 _        = True
longerThan n (_ : xs) = longerThan (n - 1) xs

balanceLeft :: FocusCircle a -> FocusCircle a
balanceLeft (FC ls f rs) = FC (ls ++ reverse rs) f []

moveLeft :: FocusCircle a -> FocusCircle a
moveLeft c@(FC [] _ [])     = c -- Can't change focus in a circle with a single element.
moveLeft (FC (l : ls) f rs) = FC ls l (f : rs)
moveLeft c@(FC [] _ _)      = moveLeft $ balanceLeft c

moveRight :: FocusCircle a -> FocusCircle a
moveRight c@(FC [] _ [])     = c -- Can't change focus in a circle with a single element.
moveRight (FC ls f (r : rs)) = FC (f : ls) r rs
moveRight c@(FC _ _ [])      = moveRight $ balanceRight c
