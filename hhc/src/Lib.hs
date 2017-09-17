module Lib

    ( sha1bf
    ) where

import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Digest.Pure.SHA
import           Data.List
import           Data.Maybe
import           Charset

-- charset, min len, max len, cipher -> (hash, maybe plain text)
sha1bf :: String -> Int -> Int -> [String] -> [(String, String)]
sha1bf charset minl maxl hashes =
  let
    digest e = showDigest $ sha1 $ C.pack $ genPlain charset e
    foundIndices = foldl (\a e -> if isJust (elemIndex (digest e) hashes) then e:a else a) [] [lowerLimit (length charset) minl .. upperLimit (length charset) maxl]
    plains = map (genPlain charset) foundIndices
  in
    zip plains $ map showDigest $ map sha1 $ map C.pack $ plains






