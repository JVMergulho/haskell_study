fibonacci:: [Int]
fibonacci = [fib x | x <- [0..]]

fib:: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)