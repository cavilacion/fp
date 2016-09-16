minThree :: Int -> Int -> Int -> Int
minThree x y z 
 | x <= y && y <= z  = x
 | y <= x && x <= z  = y
 | otherwise = z
