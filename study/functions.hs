inRange :: Integer -> Integer -> Integer -> Bool
inRange max min x =
  x >= min && x <= max

-- let bindings are used to bind variables to values in a local scope.
inRange2 :: Integer -> Integer -> Integer -> Bool
inRange2 max min x =
  let inLowerBound = x >= min
      inUpperBound = x <= max
   in inLowerBound && inUpperBound

-- where bindings are similar to let bindings, but they are used at the end of a function definition.
inRange3 :: Integer -> Integer -> Integer -> Bool
inRange3 max min x =
  inLowerBound && inUpperBound
  where
    inLowerBound = x >= min
    inUpperBound = x <= max

-- maybe is a type that can be used to represent a value that may or may not be present.
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just $ div x y