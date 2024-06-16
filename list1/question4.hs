-- 4. Dada uma matriz representada por uma lista de listas, defina fun ̧c ̃oes para:
-- (a) indicar se a mesma  ́e uma matriz (se todas as linhas tˆem o mesmo tamanho)
-- (b) permutar a posi ̧c ̃ao de duas linhas x e y, assumindo que x < y. Dica: pode-se utilizar as fun ̧c ̃oes
-- init, take, drop e !! .

checkMatrix::[[a]] -> Bool
checkMatrix list = foldr (\l acc -> (length l == length(head list)) && acc) True list

-- same as above but using filter
checkMatrix2::Eq a => [[a]] -> Bool
checkMatrix2 list = filter (\l -> length l == length(head list)) list == list

permute::[[a]] -> Int -> Int -> [[a]]
permute list x y = take x list ++ [list !! y] ++ take (y - x - 1) (drop (x + 1) list) ++ [list !! x] ++ drop (y+1) list

main :: IO ()
main = do
    let matrix = [[1,2,3], [4,5,6], [7,8,9], [10,11,12]]
    let result = permute matrix 1 3
    print result