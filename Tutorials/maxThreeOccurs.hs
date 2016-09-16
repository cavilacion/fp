maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer,Integer)
maxThreeOccurs x y z = 
	if x<y then 
		if y<z then
			(z,1)
		else if y==z then
			(z,2)
		else
			(y,1)
	else if x==y then
		if x<z then
			(z,1) 
		else if x==z then
			(z,3)
		else 
			(x,2)
	else 
		if x>z then
			(x,1)
		else if x==z then
			(x,2)
		else
			(z,1)
