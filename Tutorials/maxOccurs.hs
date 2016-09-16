maxOccurs :: Integer -> Integer -> (Integer,Integer)
maxOccurs x y = if x>y then (x,1) else if x==y then (x,2) else (y,1) 