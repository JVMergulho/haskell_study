-- 3. Considere uma fun ̧c ̃ao polinomial de grau 2 (f(x) = ax2 + bx + c), onde a, b e c s ̃ao os coeficientes
-- do polinomio.

-- (a) Defina a funcao poli :: Integer −> Integer −> Integer −> Integer −> Integer que re-
-- cebe como argumentos os coeficientes de uma fun ̧c ̃ao polinomial de grau 2 e devolve uma funcao

-- de inteiro para inteiro (um polinomio)
-- (b) Defina a funcao listaPoli :: [( Integer,Integer,Integer)] −> [Integer−>Integer] que aguarda
-- uma lista de triplas de inteiros (coeficientes de um polinomio de segundo grau) e devolve uma
-- lista de funcoes de inteiro para inteiro (polinomios) .
-- (c) Defina a funcao appListaPoli :: [Integer−>Integer] −> [Integer] −> [Integer] que recebe
-- uma lista de funcoes de polinomios e uma lista de inteiros. Esta funcao devolve uma lista
-- de inteiros que resultam da aplicacao de cada polinomio da primeira lista aplicada ao inteiro
-- correspondente na segunda lista.

poli :: Integer -> Integer -> Integer -> Integer -> Integer
poli a b c x = a*x^2 + b*x + c

listaPoli :: [( Integer,Integer,Integer)] -> [Integer ->Integer]
listaPoli list = map(\(a, b, c) -> poli a b c) list

appListaPoli :: [Integer -> Integer] -> [Integer] -> [Integer]
appListaPoli polis xs = zipWith (\f x -> f x) polis xs -- apply each function in the list to each element in the list.
appListaPoli polis xs = map(\(f, x) -> f x) $ zip polis xs -- same as above, but using map and zip instead of zipWith.

main :: IO ()
main = do
    let coefficients = [(2,3,4), (1,2,3), (4,5,6)]
    let polynomials = listaPoli coefficients
    let values = [1, 2, 3]
    let results = appListaPoli polynomials values
    print results