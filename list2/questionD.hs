-- Defina uma função recursiva para calcular o máximo divisor comum de dois números 
-- não negativos a e b, usando o algoritmo de Euclides.

mdc:: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (mod a b) 