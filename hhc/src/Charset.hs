module Charset
    ( genPlain,
    maybeGenPlain,
    lowerLimit,
    upperLimit
    ) where

import           Data.List
import           Data.Maybe


-- charset, index -> plain text
maybeGenPlain:: String -> Maybe Int -> Maybe String
maybeGenPlain c i = if isJust i then Just (genPlain c $ fromJust i) else Nothing


-- charset, index -> plain text
genPlain :: String -> Int -> String
genPlain c i =
  if (i < length c) then getchar else prefix ++ getchar
    where
        qr = i `divMod` length c
        prefix = genPlain c ((fst qr) - 1)
        getchar = c !! (snd qr):[]

lowerLimit :: Int -> Int -> Int
lowerLimit charsetLen plainLen = sum [ charsetLen ^ x | x <- [0..plainLen - 1]] - 1

upperLimit :: Int -> Int -> Int
upperLimit charsetLen plainLen = sum [ charsetLen ^ x | x <- [0..plainLen - 1]]
