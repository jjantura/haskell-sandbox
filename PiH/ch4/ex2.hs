third :: [a] -> Maybe a
third xs = if length xs > 2 then Just (head $ drop 2 xs) else Nothing
