import Data.List

-- Consider the list [a,b,c,d,e]
-- a is the head
-- [b,c,d,e] is the tail
-- e is the last
-- [a,b,c,d] is the init

takeList :: Integer -> [a] -> [a]
takeList 0 _ = []
takeList _ [] = []
takeList n (x:xs) = x : takeList (n - 1) xs

sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- The foldr function is a higher-order function that applies a function 
-- to each element of a list from right to left.

sumList2 :: Num a => [a] -> a
sumList2 xs = foldr (+) 0 xs -- start at 0 and add each element of the list

-- makeList 1 5 returns [1, 2, 3, 4, 5]
-- makeList 5 1 returns [5, 4, 3, 2, 1]
makeList:: Int -> Int -> [Int]
makeList n m
    | n > m = desc n m
    | n < m = asc n m
    | otherwise = [n]
    where 
        desc n m
            | n > m = n : desc (n - 1) m
            | otherwise = [m]
        asc n m
            | n < m = n : asc (n + 1) m
            | otherwise = [m]

-- add elements of a list of pairs using list comprehension
addPairs:: [(Int, Int)] -> [Int]
addPairs xs = [a + b | (a, b) <- xs] 
