
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