module Bruteforce

    ( bruteforce
    ) where

import           Crypto.Hash             (hashWith, HashAlgorithm (..))
import           Data.List
import           Data.Maybe
import           Data.Text               (pack)
import           Data.Text.Encoding      (encodeUtf8)
import           Charset


-- charset, min len, max len, cipher -> (hash, maybe plain text)
bruteforce :: HashAlgorithm algorithm => algorithm -> String -> Int -> Int -> [String] -> [(String, String)]
bruteforce algorithm charset minl maxl hashes =
  let
    plainToDigest e = show $ hashWith algorithm $ encodeUtf8 $ pack e
    digest e = plainToDigest $ genPlain charset e
    foundIndices = foldl' (\a e -> if isJust (elemIndex (digest e) hashes) then e:a else a) [] [lowerLimit (length charset) minl .. upperLimit (length charset) maxl]
    plains = map (genPlain charset) foundIndices
  in
    zip plains $ map plainToDigest plains
