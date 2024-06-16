-- (a) Estabele ̧ca explicitamente que o tipo Voto  ́e uma instˆancia da classe Eq de tal forma que dois
-- votos s ̃ao iguais se os c ́odigos forem iguais, considerando-se o mesmo cargo em disputa
-- (b) Defina uma fun ̧c ̃ao que retorna o total de votos de um candidato.
-- (c) Defina uma fun ̧c ̃ao que retorna o total de votos de cada candidato, ou seja, uma apura ̧c ̃ao.

import Data.List (nub)

type Codigo = Int
data Voto = Presidente Codigo | Senador Codigo | Deputado Codigo| Branco deriving (Show) 

instance Eq Voto where
    (==) :: Voto -> Voto -> Bool
    (Presidente a) == (Presidente b) = a == b
    (Senador a) == (Senador b) = a == b
    (Deputado a) == (Deputado b) = a == b
    Branco == Branco = True
    _ == _ = False

type Urna = [Voto]
type Apuracao = [(Voto , Int)]

totalVotos :: Urna -> Voto -> Int
totalVotos  urna voto = foldr(\x acc -> if x == voto then acc + 1 else acc) 0 urna

apurar :: Urna -> Apuracao
apurar urna = do
    let aux = foldr(\x acc -> (x, totalVotos urna x) : acc) [] urna
    tirarRepetidos aux
    where
        tirarRepetidos :: Eq a => [(a, b)] -> [(a, b)]
        tirarRepetidos [] = []
        tirarRepetidos (x:xs) = if any (\(v, _) -> v == fst x) xs then tirarRepetidos xs else x : tirarRepetidos xs

apurar2 :: Urna -> Apuracao
apurar2 urna = do
    let aux = foldr(\x acc -> (x, totalVotos urna x) : acc) [] urna
    nub aux -- nub is a function that removes duplicates from a list

main::IO()
main = do
    let urna = [Presidente 10, Presidente 10, Senador 20, Senador 20, Deputado 30, Branco]
    print $ apurar urna
