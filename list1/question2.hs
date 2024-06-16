-- 2. Defina uma função sublistas :: [a] −> [[a]] que retorna todas as sublistas de uma lista dada como argumento.

sublists :: [a] -> [[a]]
sublists [] = [[]] -- Return an empty list as a sublist of an empty list.
sublists (x:xs) = sublists xs ++ map (x:) (sublists xs) -- Recursively generate sublists by either excluding or including the first element.