halve :: [a] -> ([a], [a])
halve xs = if even $ length xs then (take h xs, drop h xs) else error "must be even len"  where h = (length xs) `div` 2
