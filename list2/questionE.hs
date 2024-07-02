-- Dada uma lista de inteiros, essa função retorna uma nova lista, apenas com os elementos da lista original 
-- que não são divisores exatos de qualquer dos elementos posteriores a eles.

primos :: [Int] -> [Int]
primos [] = []
primos [x] = [x]
primos (x:xs) 
    | any (\n -> n `mod` x == 0) xs = primos xs
    | otherwise = x : primos xs

-- if foldr (\x acc -> mod x (head xs) == 0 || acc) False (tail xs) then primos (tail xs) else head xs : primos (tail xs)

primosN :: Int -> [Int]
primosN n = primos [1..n]