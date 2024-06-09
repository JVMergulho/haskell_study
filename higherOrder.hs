-- anonymous function is a function that is not bound to an identifier.

-- map function is a higher-order function that applies a function to each element of a list.
addPairsMp :: [(Int, Int)] -> [Int]
addPairsMp = map (\(x, y) -> x + y)

-- The filter function is a higher-order function that processes a list 
-- and returns a new list that contains only the elements that satisfy a predicate.
filterEven:: [Int] -> [Int]
filterEven = filter (\x -> x `mod` 2 == 0)

filterDiff :: [(Int, Int)] -> [(Int, Int)]
filterDiff = filter (\(x, y) -> x /= y)