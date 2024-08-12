-- João Vitor Lima Mergulhão
-- EE1 de PLC 2024.1

-- Questão 1
-- a)
type Pessoa = String
type ID = String
type Equipe = [Pessoa]
type Projeto = (ID, Equipe)
type Projetos = [Projeto]
exemplo:: Projetos
exemplo = [("p1", ["a", "b"]), ("p2", ["a", "b", "c"])]
-- b)
criaProjeto:: String -> String -> Projetos -> Projetos
criaProjeto id pessoa projetos = if any(\(x, _) -> x == id) projetos

then projetos
else (id, [pessoa]):projetos

-- c)
equipe:: ID -> Projetos -> Equipe
equipe id projetos = if length filtra == 0 then [] else head filtra
where filtra = [ y | (x, y) <- projetos, x == id]
-- Se não existir o id na lista "filtra" é vazio
-- outra forma: equipe id projetos = head filter (\(x, _) -> x == id) projetos
-- d
naEquipe:: ID -> Pessoa -> Projetos -> Bool
naEquipe id pessoa projetos = elem pessoa (equipe id projetos) -- checa se a pessoa está
presente na lista
-- e
-- encontra índice do projeto que tem o id especificado
findIndex:: ID -> Projetos -> Int
findIndex id ((x, _):ps) = if x == id then 0 else 1 + findIndex id ps
acrescentarPessoa:: ID -> Pessoa -> Projetos -> Projetos
acrescentarPessoa id pessoa projetos
| equipe id projetos == [] = projetos
| otherwise = (take (findIndex id projetos) projetos) ++ [(id, pessoa:(equipe id projetos))]
++ (drop ((findIndex id projetos) +1 ) projetos)
-- Questão 2

data Nat = Zero | Succ Nat deriving (Eq, Show)
-- a
int2Nat:: Int -> Nat
int2Nat n = case n of
0 -> Zero
otherwise -> Succ (int2Nat (n-1))
-- b
nat2Int:: Nat -> Int
nat2Int n = case n of
Zero -> 0
Succ nt -> 1 + nat2Int nt
-- c
soma:: Nat -> Nat -> Nat
soma n1 n2 = int2Nat ((nat2Int n1) + (nat2Int n2))
-- d
somaInt:: Nat -> Nat -> Int
somaInt n1 n2 = (nat2Int n1) + (nat2Int n2)
-- outra abordagem:
-- soma:: Nat -> Nat -> Nat
-- soma n1 n2 = case (n1,n2) of
-- (Zero, _) -> Zero
-- (_, Zero) -> Zero
-- (_, Succ x) -> soma (Succ n1) x