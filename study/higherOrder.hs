import Data.List

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

-- composition of functions: 
-- f . g = \x -> f (g x)
-- (.) :: (b -> c) -> (a -> b) -> a -> c

descSort :: Ord a => [a] -> [a]
descSort = reverse . sort

squareEven :: [Int] -> [Int]
squareEven = map (\x -> x * x) . filter even 
-- even is equivalent to \x -> x `mod` 2 == 0