addBitsToList :: [String] -> Int -> [String]
addBitsToList s n
 | n==0 = s
 | otherwise = addBitsToList (['0':t | t <- s, head s == '1']) (n-1) ++ addBitsToList (['1':t | t<-s]) (n-1)