data Lampada = Compacta Nome Potencia | Incandescente Nome Potencia deriving (Read)
type Nome = String
type Potencia = Int

instance Show Lampada where
  show (Compacta n p) = "Compacta " ++ n ++ " " ++ show p
  show (Incandescente n p) = "Incandescente " ++ n ++ " " ++ show p