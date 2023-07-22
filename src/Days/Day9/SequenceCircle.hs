{-# LANGUAGE InstanceSigs #-}

module Days.Day9.SequenceCircle (SequenceCircle) where

import           Data.Sequence    (Seq)
import qualified Data.Sequence    as Seq
import           Days.Day9.Circle (Circle (..))

data SequenceCircle a = FC (Seq a) Int

instance Circle SequenceCircle where
  singleton :: a -> SequenceCircle a
  singleton = singleton'
  focus :: SequenceCircle a -> a
  focus = focus'
  moveFocus :: Int -> SequenceCircle a -> SequenceCircle a
  moveFocus = moveFocus'
  insert :: a -> SequenceCircle a -> SequenceCircle a
  insert = insert'
  remove :: SequenceCircle a -> (a, SequenceCircle a)
  remove = remove'

singleton' :: a -> SequenceCircle a
singleton' x = FC (Seq.singleton x) 0

focus' :: SequenceCircle a -> a
focus' (FC xs focus'') = Seq.index xs focus''

moveFocus' :: Int -> SequenceCircle a -> SequenceCircle a
moveFocus' n (FC xs focus'') = FC xs ((focus'' + n) `mod` Seq.length xs)

insert' :: a -> SequenceCircle a -> SequenceCircle a
insert' x (FC xs focus'') = FC (Seq.insertAt focus'' x xs) focus''

remove' :: SequenceCircle a -> (a, SequenceCircle a)
remove' c@(FC xs focus'') = (focus' c, FC (Seq.deleteAt focus'' xs) focus'')
