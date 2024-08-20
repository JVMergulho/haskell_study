import Control.Concurrent

f:: Int -> INt -> MVar Int -> IO()
f a b mVar = do  
    putMVar mVar (a+b)

main::IO()
main = do
    mVar <- newEmptyMVar
    forkIO $ f 1 2 mVar
    result <- takeMVar mVar
    putStrLn $ show result