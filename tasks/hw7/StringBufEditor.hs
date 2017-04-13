module Main where

import StringBuffer
import Editor
import Scrabble
import Sized
import JoinList

main = runEditor editor $ Single (Score 0, Size 0) "" -- VERY slow
