-- Defina um tipo chamado conta 2. Conta constitu ́ıdo de uma vari ́avel transacional do tipo inteiro. Defina
-- as seguintes fun ̧c ̃oes em Haskell:
-- (a) saque :: Conta− > Int− > STM() que realiza retirada de uma quantia de uma conta
-- (b) deposito :: Conta− > Int− > STM() que realiza um dep ́osito em uma conta. Utilize a fun ̧c ̃ao
-- saque para definir deposito
-- (c) saque2, uma modifica ̧c ̃ao da fun ̧c ̃ao saque, que bloqueia, caso o saldo da conta v ́a se tornar
-- negativo.
-- (d) Suponha que você possa retirar dinheiro de uma conta A se esta tiver saldo suficiente, senão, retira de uma conta B. Defina a função saque3, utilizando a função orElse e a função saque2.

import Control.Concurrent.STM
import Control.Concurrent

type Conta = TVar Int

-- a)
saque :: Conta -> Int -> STM()
saque conta quantia = do
    saldo <- readTVar conta
    writeTVar conta (saldo - quantia)

-- b)
deposito :: Conta -> Int -> STM()
deposito conta quantia = saque conta (-quantia)

-- c)
saque2 :: Conta -> Int -> STM()
saque2 conta quantia = do
    saldo <- readTVar conta
    if saldo - quantia < 0
    then retry
    else writeTVar conta (saldo - quantia)

-- d)
saque3 :: Conta -> Conta -> Int -> STM()
saque3 contaA contaB quantia = do
    orElse (saque2 contaA quantia) (saque2 contaB quantia)

main:: IO() 
main = do
    conta <- atomically $ newTVar 400 -- newTVarIO 100
    atomically $ deposito conta 100
    forkIO $ atomically $ saque2 conta 600
    forkIO $ atomically $ deposito conta 1000
    threadDelay 1000000
    saldo <- atomically $ readTVar conta
    print saldo
    return ()