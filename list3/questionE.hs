unzip' :: [(a,b)] -> ([a],[b])
unzip' xs = foldr (\(a,b) (as, bs) -> (a:as, b:bs)) ([], []) xs