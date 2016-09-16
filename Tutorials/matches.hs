matches :: Integer -> [Integer] -> [Integer]
matches n a = [x | x <- a, x == n]