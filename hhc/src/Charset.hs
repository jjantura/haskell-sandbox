module Charset
    ( genplain,
    lowerLimit,
    upperLimit
    ) where

import           Data.List

-- charset, index -> plain text
genplain :: String -> Int -> String
genplain c i =
  if (i < length c) then getchar else prefix ++ getchar
    where
        qr = i `divMod` length c
        prefix = genplain c ((fst qr) - 1)
        getchar = c !! (snd qr):[]

lowerLimit :: Int -> Int -> Int
lowerLimit charsetLen plainLen = sum [ charsetLen ^ x | x <- [0..plainLen - 1]] - 1

upperLimit :: Int -> Int -> Int
upperLimit charsetLen plainLen = sum [ charsetLen ^ x | x <- [0..plainLen - 1]]
