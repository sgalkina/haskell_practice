{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

--Helper function for parseMessage
parseWords :: [String] -> LogMessage
parseWords ("E":number:xs) = LogMessage (Error (read number :: Int)) (read (head xs) :: Int) (unwords (tail xs))
parseWords ("I":number:xs) = LogMessage Info (read number :: Int) (unwords xs)
parseWords ("W":number:xs) = LogMessage Warning (read number :: Int) (unwords xs)
parseWords x = Unknown (unwords x)

--Parses an individual line from the log file
parseMessage :: String -> LogMessage
parseMessage s = parseWords (words s)

--Parses an entire log file
parse :: String -> [LogMessage]
parse x = map parseMessage (lines x)

--Get time from LogMessage
time :: LogMessage -> Int
time (Unknown _) = 0
time (LogMessage _ t _) = t

--Inserts a new LogMessage into an existing MessageTree, producing a new MessageTree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert x Leaf = Node Leaf x Leaf
insert x (Node l m r)
  | (time x) >= (time m) = Node l m (insert x r)
  | otherwise = Node (insert x l) m r

--Build a complete MessageTree from a list of messages
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

--Takes a sorted MessageTree and produces a list of all the
--LogMessages it contains, sorted by timestamp from smallest to biggest
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = (inOrder l) ++ [m] ++ (inOrder r)

--Helper for whatWentWrong
filterErrors :: LogMessage -> Bool
filterErrors (LogMessage (Error x) _ _) = x >= 50
filterErrors _ = False

--Helper for whatWentWrong
getString :: LogMessage -> String
getString (LogMessage _ _ s) = s
getString (Unknown s) = s

--Takes an unsorted list of LogMessages, and returns a list of the
--messages corresponding to any errors with a severity of 50 or greater,
--sorted by timestamp
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = map getString (filter filterErrors (inOrder (build x)))

main :: IO ()
main = do
    msgs <- testWhatWentWrong parse whatWentWrong "data/error.log"
    print msgs
