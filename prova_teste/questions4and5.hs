type Pilha t = [t] 

data Elemento = Valor Int | Soma | Multiplica deriving (Show)

exemplo:: Pilha Elemento
exemplo = [Valor 10, Valor 20, Soma, Valor 30, Multiplica]

gera_string::Pilha Elemento -> String
gera_string pilha = gera $ reverse pilha
    where 
        gera [] = ""
        gera (Soma: Valor v: xs) =  "(" ++ gera xs ++ "+" ++ show v ++ ")"
        gera (Multiplica: Valor v: xs) = "(" ++ gera xs ++ "*" ++ show v ++ ")"
        gera (Valor v: xs) =  show v 
        gera _ = ""

calcula::Pilha Elemento -> Int
calcula [] = 0
calcula [Valor v] = v
calcula (Valor v1:Valor v2:Soma: xs) = calcula ((Valor (v1 + v2)):xs)
calcula (Valor v1:Valor v2:Multiplica: xs) = calcula ((Valor (v1 * v2)):xs)