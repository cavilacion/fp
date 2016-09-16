numberToDigits :: [Int] -> [Int]
numberToDigits n
 | m < 10   = n  -- base case
 | otherwise = numberToDigits ((m `div` 10):(m `mod` 10):tail n)
 where m = head n

sumSquaredDigits :: Int -> Int
sumSquaredDigits n = sum [x^2 | x <- numberToDigits [n]]

-- based on wikipedia article on happy numbers
-- every unhappy number reaches n = 4
isHappy :: Int -> Int 
isHappy n
 | n == 4    = 0    -- unhappy
 | n == 1    = 1    -- happy
 | otherwise = isHappy (sumSquaredDigits n)

countHappies :: Int -> Int -> Int
countHappies a b = sum [isHappy w | w <-[a..b]] 