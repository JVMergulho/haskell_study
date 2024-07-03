type Pilha t = [t] 

data Elemento = Valor Int | Soma | Multiplica deriving (Show)

exemplo:: Pilha Elemento
exemplo = [Valor 10, Valor 20, Soma, Multiplica]

gera_string::Pilha Elemento -> String
gera_string [] = ""
gera_string (x:y:xs) = case (x,y) of
    (Valor v1, Valor v2) -> show v1 ++ gera_string (tail xs) ++ show v2
