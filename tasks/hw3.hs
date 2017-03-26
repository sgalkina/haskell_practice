--http://www.cis.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf

--Take every Nth element from the list
everyNth :: Int -> [a] -> [a]
everyNth n xs = case (drop (n - 1) xs) of
  [] -> []
  (y:ys) -> y : everyNth n ys

skips :: [a] -> [[a]]
skips [] = []
skips xs = [everyNth n xs | n <- [1..(length xs)]]

--Get the list of all local maximas
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:rest)
  | (y > x) && (y > z) = y : localMaxima (z:rest)
  | otherwise = localMaxima (y:z:rest)
localMaxima xs = []

-- --Outputs a vertical histogram showing how many of each number
-- --were in the input list
-- histogram :: [Integer] -> String

main = print (localMaxima [2,9,5,6,1])
