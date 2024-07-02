-- Defina uma função expNE que calcule o resultado da exponenciação inteira de X elevado à Y

expNE::Float -> Int -> Float
expNE x 0 = 1
expNE x y = x * (expNE x (y-1))