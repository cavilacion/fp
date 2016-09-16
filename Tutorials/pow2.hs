pow2 :: Integer -> Integer
pow2 n 
 | n == 0 = 1
 | (n `mod` 2) == 0    = (pow2 (n `div` 2)) * (pow2 (n `div` 2))
 | (n `mod` 2) == 1    = 2* (pow2 (n `div` 2)) * (pow2 (n `div` 2))
testPow2 :: Integer -> Bool
testPow2 n = n<0 || pow2 n == 2^n