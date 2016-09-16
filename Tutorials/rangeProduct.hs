rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n
 | m == n = m
 | m > n = 0
 | n > m = m * rangeProduct (m+1) n 
rangeProduct2 :: Integer -> Integer -> Integer
rangeProduct2 m n = if n < m then 0 else product [m..n]