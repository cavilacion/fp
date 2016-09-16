nAnd :: Bool -> Bool -> Bool
nAnd a b 
 | a && b = False
 | a && not b = True
 | not a && b = True
 | not a && not b = True