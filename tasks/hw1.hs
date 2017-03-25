-- Convert positive Integer to a list of digits
toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = toDigits (div x 10) ++ [mod x 10]

-- Convert positive Integer to a reversed list of digits
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = (mod x 10) : toDigitsRev (div x 10)

-- Double every other number
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x : y * 2 : doubleEveryOther zs

--Calculate the sum of all digits
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

--Indicates whether an Integer could be a valid credit card number
validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigitsRev x))) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
--Given the number of discs and names for the three pegs, hanoi
--should return a list of moves to be performed to move the stack of
--discs from the first peg to the second
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi 1 a b c = [(a, b)]
hanoi x a b c = hanoi (x - 1) a c b ++ hanoi 1 a b c ++ hanoi (x - 1) c b a

main = print (hanoi 3 "a" "b" "c")
