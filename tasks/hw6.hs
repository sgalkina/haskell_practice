{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
--Translate the definition of Fibonacci numbers directly into a recursive function definition of type
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (+) (fib (n - 1)) (fib (n - 2))

--Use fib to define the infinite list of all Fibonacci numbers
--You will probably get bored watching it after the first 30 or so, because fib is ridiculously slow
fibs1 :: [Integer]
fibs1 = map fib [0..]

--Define the infinite list so that it has the same elements as fibs1,
--but computing the first n elements of fibs2 requires only O(n) addition operations
fibs2 :: [Integer]
fibs2 = fibn 0 1
  where fibn x y = x : fibn y (x + y)

--Define a data type of polymorphic streams, Stream
data Stream a = Cons a (Stream a)  --element followed by stream of elements

--Write a function to convert a Stream to an infinite list
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:(streamToList xs)

--Make your own instance of Show for Stream which works by showing only some prefix of a stream
instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

--Generates a stream containing infinitely many copies of the given element
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

--Applies a function to every element of a Stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

--Generates a Stream from a “seed” of type a, which is the first element
--of the stream, and an “unfolding rule” of type a -> a which specifies
--how to transform the seed into a new seed, to be used for generating
--the rest of the stream
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

--Contains the infinite list of natural numbers 0, 1, 2, . . .
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ~(Cons y ys) = Cons x $ Cons y $ interleaveStreams xs ys

--nth element in the stream is the largest power of 2 which evenly divides n.
ruler :: Stream Integer
ruler = foldr1 interleaveStreams $ map streamRepeat [0..]

--Fibonacci numbers via generating functions

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger x = Cons x $ streamRepeat 0
  negate = streamMap (*(-1))
  (+) (Cons x xs) (Cons y ys) = Cons (x + y) $ (+) xs ys
  (*) (Cons x xs) b@(Cons y ys) = Cons (x * y) $ ((fromInteger x) * ys) + (xs * b)

instance Fractional (Stream Integer) where
    (/) (Cons x xs) b@(Cons y ys) = q where
        divide a = floor (fromIntegral a / fromIntegral y::Double)
        coef = floor (fromIntegral x / fromIntegral y::Double)
        q = Cons coef (streamMap divide (xs - (q * ys)))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)
