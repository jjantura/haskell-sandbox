module Hex (
    toHex
) where

import Data.Word

hexChars :: [Char]
hexChars = "0123456789ABCDEF"

toHex :: Word8 -> String
toHex n = if n < 16 then [hexChars !! fromIntegral n] else (toHex $ n `div` 16) ++ [hexChars !! (fromIntegral n `mod` 16)]
