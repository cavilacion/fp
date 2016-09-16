-- Exercise 1: Happy Numbers
countHappyNumbers :: Int -> Int -> Int
countHappyNumbers a b =
  length [x | x <- [a .. b], checkHappyNumber x]

checkHappyNumber :: Int -> Bool
checkHappyNumber a
  | a == 1 = True
  | a == 4 = False -- A number either ends in 1, or a cycle containing 4
  | otherwise = checkHappyNumber (getSumSquareDigits a)


getSumSquareDigits :: Int -> Int
getSumSquareDigits x
  | x `div` 10 == 0 = ((x `mod` 10) * (x `mod` 10))
  | otherwise = ((x `mod` 10) * (x `mod` 10)) + getSumSquareDigits (x `div` 10)



-- Exercise 2: Takuzu Bitstrings 
takuzuStrings :: Int -> [[Char]]
takuzuStrings n

checkTakuzu :: [Char] -> Bool
checkTakuzu string

  | n == 0 = True
  | otherwise = '1' ++ checkTakuzu(n-1)
