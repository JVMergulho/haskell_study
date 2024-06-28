merge:: Ord a => [a] -> [a] -> [a]
merge [] right = right
merge left [] = left
merge left@(l:ls) right@(r:rs)
    | l <= r    = l : merge ls right
    | otherwise = r : merge left rs
