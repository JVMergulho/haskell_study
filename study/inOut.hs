greet::IO ()

greet = do
    putStr "What is your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name)

main::IO ()
main = do
    i <- getLine
    if i /= "quit" then do
        greet
        main
    else 
        return ()