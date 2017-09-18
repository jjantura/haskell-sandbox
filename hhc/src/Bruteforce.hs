module Bruteforce

    ( sha1bf
    ) where

import           Crypto.Hash             (hashWith, SHA1 (..))
import           Data.List
import           Data.Maybe
import           Data.Text               (pack)
import           Data.Text.Encoding      (encodeUtf8)
import           Charset


-- charset, min len, max len, cipher -> (hash, maybe plain text)
sha1bf :: String -> Int -> Int -> [String] -> [(String, String)]
sha1bf charset minl maxl hashes =
  let
    plainToDigest e = show $ hashWith SHA1 $ encodeUtf8 $ pack e
    digest e = plainToDigest $ genPlain charset e
    foundIndices = foldl' (\a e -> if isJust (elemIndex (digest e) hashes) then e:a else a) [] [lowerLimit (length charset) minl .. upperLimit (length charset) maxl]
    plains = map (genPlain charset) foundIndices
  in
    zip plains $ map plainToDigest plains
