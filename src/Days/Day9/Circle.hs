module Days.Day9.Circle (Circle(..)) where

class Circle c where
  singleton :: a -> c a
  focus :: c a -> a
  moveFocus :: Int -> c a -> c a
  insert :: a -> c a -> c a
  remove :: c a -> (a, c a)
