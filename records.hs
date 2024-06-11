-- records in Haskell are similar to structs in C
-- they are used to group related data together

data Person = Person { name :: String, age :: Int} deriving (Show)

greet:: Person -> String
greet person = "Hello, " ++ name person ++ "!"

p :: Person
p = Person { name = "Joao", age = 21 }
