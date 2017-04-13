{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module StringBuffer where

import Data.Monoid
import Data.List

import Buffer
import Scrabble
import Sized
import JoinList

instance Buffer String where
  toString     = id
  fromString   = id
  line n b     = safeIndex n (lines b)
  replaceLine n l b = unlines . uncurry replaceLine' . splitAt n . lines $ b
      where replaceLine' pre [] = pre
            replaceLine' pre (_:ls) = pre ++ l:ls
  numLines     = length . lines
  value        = length . words

numOfLines :: (Score, Size) -> Int
numOfLines (_, s) = getSize s

textValue :: (Score, Size) -> Int
textValue (s, _) = getScore s

instance Buffer (JoinList (Score, Size) String) where
  toString l   = intercalate "\n" (jlToList l)
  fromString s = foldr (+++) Empty (map (\x -> Single (scoreString s, 1) s) (lines s)) --TODO: balanced tree
  line n b     = indexJ n b
  replaceLine n l b = (takeJ (n - 1) b) +++ (Single (scoreString l, 1) l) +++ (dropJ (n + 1) b)
  numLines     = numOfLines . tag
  value        = textValue . tag

safeIndex :: Int -> [a] -> Maybe a
safeIndex n _ | n < 0 = Nothing
safeIndex _ []        = Nothing
safeIndex 0 (x:_)     = Just x
safeIndex n (_:xs)    = safeIndex (n-1) xs
