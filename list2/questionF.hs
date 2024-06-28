-- Defina a função perfeitos :: Integer -> [Integer] que, ao receber um argumento n, 
-- retorna a lista dos números perfeitos menores ou iguais a n união {1}. Para isso, 
-- defina e use função fatores, que retorna a lista de fatores primos de seu argumento.

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree a b c
    | a >= b && a >= c = a
    | b >= a && b >= c = b
    | otherwise = c

maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour a b c d
    | maxThree a b c >= d = maxThree a b c
    | otherwise = d

maxFour' :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour' a b c d = max a (max b (max c d))

maxFour'' :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour'' a b c d = max a (maxThree b c d)