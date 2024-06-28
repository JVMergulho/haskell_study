primos :: [Int] -> [Int]
primos [] = []
primos [x] = [x]
primos (x:xs) 
    | any (\n -> n `mod` x == 0) xs = primos xs
    | otherwise = x : primos xs

-- if foldr (\x acc -> mod x (head xs) == 0 || acc) False (tail xs) then primos (tail xs) else head xs : primos (tail xs)

primosN :: Int -> [Int]
primosN n = primos [1..n]