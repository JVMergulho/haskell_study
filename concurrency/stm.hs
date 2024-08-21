import Control.Concurrent
import Control.Concurrent.STM

type Result = TVar (Int, Int)

addToResult:: Result -> Int -> STM ()
addToResult result x = do 
    (sum, counter) <- readTVar result
    writeTVar result (sum+x, counter+1)

waitForCounter:: Result -> Int -> STM Int
waitForCounter result limit = do
    (sum, counter) <- readTVar result
    if counter < limit then retry else return sum 

main::IO ()
main = do
    let n = 100
    result <- atomically $ newTVar (0,0)
    mapM_ (\x -> forkIO $ atomically $ addToResult result x) [1..n]
    sum <- atomically $ waitForCounter result n
    putStrLn $ "Sum is " ++ show sum