charToNum :: Char -> Int
charToNum a 
 | fromEnum a >= fromEnum '0' && fromEnum a <= fromEnum '9' = fromEnum a - fromEnum '0'
 | otherwise = 0