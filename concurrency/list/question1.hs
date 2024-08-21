-- Uma fábrica de porcas e parafusos funciona com o uso de uma máquina de fabricação de porcas e de uma outra máquina que fabrica parafusos. Após a produção, porcas e parafusos são colocados em caixas separadas de onde são retirados por duas máquinas que montam pares porca-parafuso.
-- Cada par de porca-parafuso é colocado em uma caixa única. Implemente, em Haskell, o que foi descrito, utilizando variáveis ​​mutáveis ​​(MVar).

-- As funções recebme um MVar extra para sincronizar a saída. Isso é necessário para que a saída não seja misturada entre as threads.

import Control.Concurrent

fabrica:: MVar Int -> String -> MVar () -> IO()
fabrica caixa item lock = loop
    where 
        loop = do
            threadDelay 500000 -- delay de 0.5s
            num <- takeMVar caixa
            withMVar lock $ \_ -> putStrLn $ "Montou " ++ item ++ " Total: " ++ show (num + 1)
            putMVar caixa (num + 1)
            loop

montaPar:: MVar Int -> MVar Int -> MVar Int -> MVar() -> IO()
montaPar caixaPorcas caixaParafusos caixaPares lock = loop
    where 
        loop = do
            threadDelay 500000 -- delay de 0.5s
            porcas <- takeMVar caixaPorcas
            parafusos <- takeMVar caixaParafusos
            pares <- takeMVar caixaPares
            if porcas > 0 && parafusos > 0 
            then do
                putMVar caixaPorcas (porcas - 1)
                putMVar caixaParafusos (parafusos - 1)
                withMVar lock $ \_ -> putStrLn $ "Montou um par. Total: " ++ show (pares + 1)
                putMVar caixaPares (pares + 1)
            else do
                putMVar caixaPorcas porcas
                putMVar caixaParafusos parafusos
            loop
    
main = do
    caixaPorcas <- newMVar 0
    caixaParafusos <- newMVar 0
    caixaPares <- newMVar 0
    lock <- newMVar () -- lock para sincronizar a saída

    forkIO $ fabrica caixaPorcas "porca" lock
    forkIO $ fabrica caixaParafusos "parafuso" lock
    forkIO $ montaPar caixaPorcas caixaParafusos caixaPares lock