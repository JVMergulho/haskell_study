balanceado :: Lustre -> Bool
balanceado (Pendente lampada) = True
balanceado (Barra l1 l2) = (potencia l1 == potencia l2) && balanceado l1 && balanceado l2