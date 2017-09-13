
module Lib

    ( sha1bf
    ) where

import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Digest.Pure.SHA
import           Data.List
import           Data.Maybe
import           Charset

-- TODO: operate on hash list instead of single hash (performance)
-- charset, min len, max len, cipher -> (hash, maybe plain text)
sha1bf :: String -> Int -> Int -> String -> (String, String)
-- sha1bf :: String -> Int -> Int -> String -> [(String, String)]
sha1bf charset minl maxl hash =
    -- let
    --   plains = [genplain charset i | i <- [lowerLimit (length charset) minl .. upperLimit (length charset) maxl]]
    --   hashes = map showDigest $ map sha1 $ map C.pack plains
    --   maybeIndex = elemIndex hash hashes
    -- in
    --   foldl (\a e -> if isJust maybeIndex then [(hash, genplain charset (fromJust maybeIndex + (lowerLimit (length charset) minl)))] else [("", "")]) [] plains
    let
      plains = [genplain charset i | i <- [lowerLimit (length charset) minl .. upperLimit (length charset) maxl]]
      hashes = map showDigest $ map sha1 $ map C.pack plains
      maybeIndex = elemIndex hash hashes
    in
      if isJust maybeIndex then (hash,  genplain charset (fromJust maybeIndex + (lowerLimit (length charset) minl))) else (hash, "")
