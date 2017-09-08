
module Lib

    ( sha1bf
    ) where

import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Digest.Pure.SHA
import           Data.List
import           Data.Maybe.Utils
import           Charset

-- charset, min len, max len, cipher -> plain text
sha1bf :: String -> Int -> Int -> String -> (String, String)
sha1bf charset minl maxl hash =
    let
      plains = [genplain charset i | i <- [lowerLimit (length charset) minl .. upperLimit (length charset) maxl]]
      hashes = map showDigest $ map sha1 $ map C.pack plains
      index = forceMaybe $ elemIndex hash hashes
    in
      (hash,  genplain charset (index + (lowerLimit (length charset) minl)))
