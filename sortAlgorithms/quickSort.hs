quickSort:: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort smaller ++ mid ++ quickSort greater
    where 
        smaller = [y | y <- xs, y < x]
        greater = [y | y <- xs, y > x]
        mid = x:[y | y <- xs, y == x]