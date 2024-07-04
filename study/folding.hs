-- folding functions are higher-order functions that process a list and return a single value.
-- foldr function applies a function to each element of a list from right to left.
-- foldl function applies a function to each element of a list from left to right.

multList :: Num a => [a] -> a
multList xs = foldr (*) 2 xs 

andList:: [Bool] -> Bool
andList xs = foldr (&&) True xs 

-- count function counts the number of occurrences of an element e in a list.
count :: Eq a => a -> [a] -> Int 
count e = 
    foldr (\x acc -> if x == e then acc + 1 else acc) 0