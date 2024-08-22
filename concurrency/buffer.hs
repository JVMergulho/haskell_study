-- Usando memória transacional, implemente um buffer em Haskell. Dado o tipo 4. Buffer que pode salvar valores de qualquer tipo, defina a função put (tipo: Buffer a −> a −> STM()) e a função get (tipo: Buffer a −> STM a). A função sempre coloca novos dados no buffer; a função get retorna um valor do buffer, mas espera se o buffer estiver vazio.

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

type Buffer a = TVar [ a ]

newBuffer:: [ a ] -> IO ( Buffer a )
newBuffer = newTVarIO

get :: Buffer a -> STM a
get buffer = do
    list <- readTVar buffer
    case list of
        [] -> retry
        (x:xs) -> do
            writeTVar buffer xs
            return x

put:: Buffer a -> a -> STM()
put buffer value = do
    list <- readTVar buffer
    writeTVar buffer (value:list)

main :: IO ()
main = do
    buffer <- newBuffer [] -- Cria um buffer vazio
    
    -- Cria uma thread que adiciona valores ao buffer
    forkIO $ atomically $ do
        put buffer 1
        put buffer 2
        put buffer 3
        put buffer 4
    
    -- Cria uma thread que lê valores do buffer
    forkIO $ forever $ do
        value <- atomically $ get buffer
        putStrLn $ "Received: " ++ show value
    
    -- Espera para garantir que todas as operações sejam executadas
    threadDelay 1000000
