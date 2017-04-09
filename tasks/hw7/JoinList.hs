module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append ((tag a) <> (tag b)) a b

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                = Nothing
indexJ i (Single _ n)         = if i == 0 then Just n else Nothing
indexJ i a@(Append _ l r)
  | i < 0 || i >= (listSize a) = Nothing
  | (listSize l) <= i          = indexJ (i - (listSize l)) r
  | otherwise                  = indexJ i l
  where listSize = getSize . size . tag

main = print (indexJ 2 t)
-- main = print ((!!?) (jlToList t) 1)
  where t = Append (Size 3) (Append (Size 2) (Single (Size 1) 2) (Single (Size 1) 4)) (Single (Size 1) 8)
