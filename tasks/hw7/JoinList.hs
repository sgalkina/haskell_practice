module JoinList where

import Data.Monoid
import Scrabble
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

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i a@(Single _ _) = if i == 0 then a else Empty
dropJ i a@(Append s l r)
  | i <= 0 = a
  | i >= (listSize a) = Empty
  | i >= (listSize l) = dropJ (i - (listSize l)) r
  | otherwise = (dropJ i l) +++ r
  where listSize = getSize . size . tag

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i a@(Single _ _) = if i == 0 then Empty else a
takeJ i a@(Append s l r)
  | i <= 0 = Empty
  | i >= (listSize a) = a
  | i >= (listSize l) = l +++ (takeJ (i - (listSize l)) r)
  | otherwise = takeJ i l
  where listSize = getSize . size . tag

score :: Char -> Score
score 'a' = 1
score 'b' = 3
score 'c' = 3
score 'd' = 2
score 'e' = 1
score 'f' = 4
score 'g' = 2
score 'h' = 4
score 'i' = 1
score 'j' = 8
score 'k' = 5
score 'l' = 1
score 'm' = 3
score 'n' = 1
score 'o' = 1
score 'p' = 3
score 'q' = 10
score 'r' = 1
score 's' = 1
score 't' = 1
score 'u' = 1
score 'v' = 4
score 'w' = 4
score 'x' = 8
score 'y' = 4
score 'z' = 10
score _ = 0

scoreString :: String -> Score
scoreString = sum . (map  score)

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- main = print (scoreLine "yay " +++ scoreLine "haskell!")
