import Control.Concurrent

f:: Int -> Int -> MVar Int -> IO()
f a b mVar = do  
    putMVar mVar $! (a+b) 
    -- exclamation mark forces the evaluation of the expression

main::IO()
main = do
    mVar <- newEmptyMVar
    forkIO $ f 1 2 mVar
    result <- takeMVar mVar
    -- takeMVar blocks until the MVar is full
    putStrLn $ show result