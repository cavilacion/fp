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
takuzuStrings n = recursiveTakuzu n ""


recursiveTakuzu :: Int -> [Char] -> [[Char]]
recursiveTakuzu n string
  | n == 0 = [reverse string]
  | (take 2 string) == "11" = recursiveTakuzu (n-1) ('0':string)
  | (take 2 string) == "00" = recursiveTakuzu (n-1) ('1':string)
  | otherwise = recursiveTakuzu (n-1) ('0':string) ++ recursiveTakuzu (n-1) ('1':string)


-- Exercise 4: Last n digits
lastDigits :: Int -> Int -> [Int]
lastDigits n d =
  drop (length digits - d) digits
  where digits = getDigits (sum [n^p | p <- [0 .. n]])

getDigits :: Int -> [Int]
getDigits x
  | x == 0 = []
  | otherwise = getDigits(x `div` 10) ++ [x `mod` 10]

-- Exercise 7: Takuzu solver
-- -- isCorrectTakuzu :: [[Char]] -> Bool
-- -- isCorrectTakuzu =
-- recursiveTakuzuSolver :: [[Char]] -> Bool
-- recursiveTakuzuSolver grid
--   | n == 0 && checkTakuzuString 8 string = True
--   with corrects takuzuStrings (length grid !! 1)


takuzuStringsCorrect :: Int -> [[Char]]
takuzuStringsCorrect n = [x | x <- recursiveTakuzu n "", length (filter (=='1') x) == (n `div` 2)]

checkTakuzuString :: Int -> [Char] -> Bool
checkTakuzuString n string = elem string (takuzuStringsCorrect n)

dotLocations :: [[Char]] -> [(Int, Int)]
dotLocations grid = [(x,y) | x <- [0..8], y <- [0..8], ((grid !! y) !! x) == '.']
