module Dictionary

    ( dictionary
    ) where

import           Charset
import           Crypto.Hash        (HashAlgorithm (..), hashWith)
import           Data.List
import           Data.Maybe
import           Data.Text          (pack)
import           Data.Text.Encoding (encodeUtf8)


-- dictionary, cipher -> (hash, maybe plain text)
dictionary :: HashAlgorithm algorithm => algorithm -> [String] -> [String] -> [(String, String)]
dictionary algorithm words hashes =
  let
    plainToDigest e = show $ hashWith algorithm $ encodeUtf8 $ pack e
    plains = foldl' (\a e -> if isJust (elemIndex (plainToDigest e) hashes) then e:a else a) [] words
  in
    zip plains $ map plainToDigest plains
