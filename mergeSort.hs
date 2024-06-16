merge:: Ord a => [a] -> [a] -> [a]
merge [] right = right
merge left [] = left
merge left@(l:ls) right@(r:rs)
    | l <= r    = l : merge ls right
    | otherwise = r : merge left rs

mergeSort:: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = merge (mergeSort left) (mergeSort right)
    where
        (left, right) = splitAt (length list `div` 2) list
        -- pivot = length list `div` 2
        -- left = take pivot list
        -- right = drop pivot list

main::IO ()
main = do
    let list = [20,6,7,1,3,5,3,4,8]
    print $ mergeSort list