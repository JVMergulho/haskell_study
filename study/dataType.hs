data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

tree :: Tree Int
tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

data Expr = Val Int | Add Expr Expr | Mul Expr Expr deriving (Show)

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

ex :: Expr
ex = Add (Val 1) (Mul (Val 2) (Val 3))