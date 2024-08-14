findDifference:: [Int] -> [Int] -> Maybe String
findDifference l1 l2
  | length l1 /= length l2 = Just $ show (length l1) ++ " /= " ++ show (length l2)
  | otherwise = case findIdx (zip l1 l2) 0 of
      Just idx -> Just $ show (l1 !! idx) ++ " /= " ++ show (l2 !! idx)
      Nothing  -> Nothing

findIdx :: [(Int, Int)] -> Int -> Maybe Int
findIdx list idx 
  | idx >= length list = Nothing
  | (a /= b) = Just idx
  | otherwise = findIdx list (idx + 1)
  where 
    (a, b) = list !! idx