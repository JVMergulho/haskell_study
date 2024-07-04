data Tree a = NilT | Node a (Tree a) (Tree a)

depth:: Tree a -> Int
depth t = case t of  
    NilT ->  0
    (Node _ t1 t2) -> max (depth t1) (depth t2) + 1

preOrderTree:: Tree a -> [a]
preOrderTree NilT = []
preOrderTree (Node v t1 t2) = v: (preOrderTree t1 ++ preOrderTree t2)

inOrderTree:: Tree a -> [a]
inOrderTree NilT = []
inOrderTree (Node v t1 t2) = inOrderTree t1 ++ [v] ++ inOrderTree t2

tree1 = Node 1 (Node 2 NilT NilT ) (Node 3 (Node 4 NilT NilT) (Node 5 NilT NilT) )