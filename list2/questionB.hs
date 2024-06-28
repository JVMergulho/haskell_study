-- Defina a função perfeitos :: Integer -> [Integer] que, ao receber um argumento n, 
-- retorna a lista dos números perfeitos menores ou iguais a n união {1}. Para isso, 
-- defina e use função fatores, que retorna a lista de fatores primos de seu argumento.

fatores:: Integer -> [Integer]
fatores 1 = []
fatores n
    | fator == [] = []
    | otherwise =  (head fator) : fatores (div n (head fator))
    where fator = filter (\x -> (n `mod` x) == 0) [2 .. n-1]

tirarRepetidos :: [Integer] -> [Integer]
tirarRepetidos [] = []
tirarRepetidos (x:xs) = x : tirarRepetidos (filter (/= x) xs)

perfeitos :: Integer -> [Integer]
perfeitos 0 = [1]
perfeitos n
    | sumSquaredFactors == n = n : perfeitos (n - 1)
    | otherwise = perfeitos (n - 1)
    where
        sumSquaredFactors = sum (map (\x -> x*x) (tirarRepetidos.fatores $ n))
