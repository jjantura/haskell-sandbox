
module Lib

    ( sha1bf
    ) where

import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Digest.Pure.SHA
import           Data.List
import           Data.Maybe.Utils

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
upperLimit charsetLen plainLen = sum [ charsetLen ^ x | x <- [0..plainLen]]

-- charset, min len, max len, cipher -> plain text
sha1bf :: String -> Int -> Int -> String -> (String, String)
sha1bf charset minl maxl hash =
    let
      plains = [genplain charset i | i <- [lowerLimit (length charset) minl .. upperLimit (length charset) maxl]]
      hashes = map showDigest $ map sha1 $ map C.pack plains
      index = forceMaybe $ elemIndex hash hashes
    in
      (hash,  genplain charset (index + (lowerLimit (length charset) minl)))
