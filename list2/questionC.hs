-- Defina função numDiv que recebe dois valores inteiros e retorna o número de vezes que uma divisão exata pode ser realizada

numDiv:: Integral a => a -> a -> a
numDiv x y
  | mod x y == 0 = 1 + numDiv (div x y) y
  | otherwise = 0