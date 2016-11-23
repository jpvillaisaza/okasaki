module Okasaki.Data.Stack
  where

-- okasaki
import Okasaki.Prelude


-- |
--
--

data Stack a
  = Nil
  | Cons a (Stack a)


-- |
--
--

empty :: Stack a
empty = Nil


-- |
--
--

isEmpty :: Stack a -> Bool
isEmpty Nil = True
isEmpty _ = False


-- |
--
--

head :: Stack a -> Maybe a
head Nil = Nothing
head (Cons x _) = Just x


-- |
--
--

tail :: Stack a -> Maybe (Stack a)
tail Nil = Nothing
tail (Cons _ xs) = Just xs


-- |
--
--

append :: Stack a -> Stack a -> Stack a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)
