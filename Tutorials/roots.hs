discriminant :: Float -> Float -> Float -> Float
discriminant a b c = b^2 - 4*a*c
smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
 | discriminant a b c >= 0 = (-b - sqrt(discriminant a b c))/(2*a)
 | otherwise = 0
largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c
 | discriminant a b c >= 0 = (-b + sqrt(discriminant a b c))/(2*a)
 | otherwise = 0
