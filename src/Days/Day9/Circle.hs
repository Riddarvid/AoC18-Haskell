module Days.Day9.Circle (Circle(..)) where

class Circle c where
  singleton :: a -> c a
  current :: c a -> a
  moveCurrent :: Int -> c a -> c a
  insert :: a -> c a -> c a
  delete :: c a -> c a
