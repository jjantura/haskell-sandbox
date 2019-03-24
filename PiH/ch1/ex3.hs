product' [] = 0
product' [x] = x
product' (x:xs) = x * product xs
