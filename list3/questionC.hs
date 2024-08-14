data Lustre = Barra Lustre Lustre | Pendente Lampada deriving (Read)

potencia :: Lustre -> Int
potencia (Barra l1 l2) = potencia l1 + potencia l2
potencia (Pendente (Compacta _ p)) = p
potencia (Pendente (Incandescente _ p)) = p