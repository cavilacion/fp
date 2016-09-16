numberNDroots :: Float -> Float -> Float -> Integer
numberNDroots a b c
 | b^2 - 4*a*c > 0 = 2
 | b^2 - 4*a*c == 0 = 1
 | b^2 - 4*a*c < 0 = 0