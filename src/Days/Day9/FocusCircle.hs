{-# LANGUAGE InstanceSigs #-}

module Days.Day9.FocusCircle (singletonFC, FocusCircle) where

import           Data.Sequence    (Seq)
import qualified Data.Sequence    as Seq
import           Days.Day9.Circle (Circle (current, delete, insert, moveCurrent, singleton))

data FocusCircle a = FC (Seq a) Int

instance Circle FocusCircle where
  singleton :: a -> FocusCircle a
  singleton = singletonFC
  current :: FocusCircle a -> a
  current = focus
  moveCurrent :: Int -> FocusCircle a -> FocusCircle a
  moveCurrent = moveFocus
  insert :: a -> FocusCircle a -> FocusCircle a
  insert = insertFC
  delete :: FocusCircle a -> FocusCircle a
  delete = deleteFC

singletonFC :: a -> FocusCircle a
singletonFC x = FC (Seq.singleton x) 0

focus :: FocusCircle a -> a
focus (FC xs focus') = Seq.index xs focus'

moveFocus :: Int -> FocusCircle a -> FocusCircle a
moveFocus n (FC xs focus') = FC xs ((focus' + n) `mod` Seq.length xs)

insertFC :: a -> FocusCircle a -> FocusCircle a
insertFC x (FC xs focus') = FC (Seq.insertAt focus' x xs) focus'

deleteFC :: FocusCircle a -> FocusCircle a
deleteFC (FC xs focus') = FC (Seq.deleteAt focus' xs) focus'
