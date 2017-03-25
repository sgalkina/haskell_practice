-- Convert positive Integers to a list of digits
toDigits :: Integer -> [Integer]
toDigits x = [x, x]

main = print (toDigits 1234)
