-- pow x n returns x to the power of n
pow :: Integer -> Integer -> Integer
pow x n 
 | n == 0  = 1 -- the base case
 | n > 0   = x * pow x (n-1) -- the recursive case
