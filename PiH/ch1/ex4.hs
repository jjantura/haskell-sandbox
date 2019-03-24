rqsort [] = []
rqsort [x] = [x]
rqsort (x:xs) = rqsort [y | y <- xs, y > x] ++ [x] ++ rqsort [z | z <- xs, z <= x]
