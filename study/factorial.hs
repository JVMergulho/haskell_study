fact:: Integer -> Integer
fact n = 
    if n == 0 then 
        1 
    else 
        n * fact (n - 1)

-- using patter matching:
fact2 :: Integer -> Integer
fact2 0 = 1
fact2 n = n * fact2 (n - 1)

-- using guards:
fact3 :: Integer -> Integer
fact3 n
    |  n == 0 = 1
    | otherwise = n * fact3 (n - 1)