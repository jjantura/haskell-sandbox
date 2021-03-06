third :: [a] -> Maybe a
third xs = if length xs > 2 then Just (head $ drop 2 xs) else Nothing

third' :: [a] -> Maybe a
third' xs = if length xs > 2 then Just (xs !! 2) else Nothing

third'' :: [a] -> Maybe a
third'' [] = Nothing
third'' [x] = Nothing
third'' [x,y] = Nothing
third'' (x:y:xs) = Just $ head xs
