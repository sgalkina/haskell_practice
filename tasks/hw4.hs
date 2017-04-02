import Data.List

--Reimplement fun1 and fun2 in a more idiomatic Haskell style
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate (\x -> if even x then (x `div` 2) else (3 * x + 1))

--Generate a balanced binary tree from a list of values using foldr
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = 0
height (Node n _ _ _) = n

insertNode :: a -> Tree a -> Tree a
insertNode x Leaf = Node 0 Leaf x Leaf
insertNode x (Node n t1 val t2)
  | h1 > h2 = Node n t1 val (insertNode x t2)
  | h1 < h2 = Node n it1 val t2
  | h1 == h2 = Node (h+1) it1 val t2
  where
    h1 = height t1
    h2 = height t2
    it1 = insertNode x t1
    h = height it1

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

--Implement a function which returns True if and only if there are an odd number of True
xor :: [Bool] -> Bool
xor = odd . length . filter (== True)

--The same using a fold
xor' :: [Bool] -> Bool
xor' = foldr (/=) False

--Implement map as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x) : y) []

-- Implement Sieve of Sundaram using function composition
cartProd :: [a] -> [(a, a)]
cartProd xs = [(x,y) | x <- xs, y <- xs]

getNumbers :: Integer -> [Integer]
getNumbers x = (\\) [1..x] ((map (\(x, y) -> x + y + (2*x*y)) . cartProd) [1..x])

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\x -> 2*x + 1) . getNumbers

main = print ( sieveSundaram 10 )
