module Bruteforce

    ( useBruteforce,
      bruteforce
    ) where

import           Charset
import           CommandLine
import           Crypto.Hash        (HashAlgorithm (..), MD5 (..), SHA1 (..),
                                     hashWith)
import           Data.Char
import           Data.List          as L
import           Data.Map           as M
import           Data.Maybe
import           Data.Text          (pack)
import           Data.Text.Encoding (encodeUtf8)
import           File


-- TODO: fix string to hash alg conversion
alg2bf :: String -> String -> Int -> Int -> [String] -> [(String, String)]
alg2bf alg =
    let lowercased = L.map toLower alg in
        case lowercased of
            "sha1" -> bruteforce SHA1
            _      -> bruteforce MD5

argList :: [String] -> [(Maybe String, Maybe String)]
argList args = L.map (takeArgValue args) ["-a", "-c", "-ll", "-ul", "-i"]

-- obligatory params
argMap :: [String] -> Map (Maybe String) (Maybe String)
argMap args = M.fromList $ argList args

useBruteforce :: [String] -> IO()
useBruteforce args =
        let am = argMap args
            alg = asString $ am ! Just "-a"
            charset = asString $ am ! Just "-c"
            minLen = asInt $ am ! Just "-ll"
            maxLen = asInt $ am ! Just "-ul"
            input = asString $ am ! Just "-i" in
            do
                content <- loadFile input
                print $ alg2bf alg charset minLen maxLen $ lines content

asInt :: Maybe String -> Int
asInt s = if isJust s then read $ fromJust s :: Int else -1

asString :: Maybe String -> String
asString = fromMaybe ""

-- charset, min len, max len, cipher -> (hash, maybe plain text)
bruteforce :: HashAlgorithm algorithm => algorithm -> String -> Int -> Int -> [String] -> [(String, String)]
bruteforce algorithm charset minl maxl hashes =
  let
    plainToDigest e = show $ hashWith algorithm $ encodeUtf8 $ pack e
    digest e = plainToDigest $ genPlain charset e
    foundIndices = L.foldl' (\a e -> if isJust (elemIndex (digest e) hashes) then e:a else a) [] [lowerLimit (length charset) minl .. upperLimit (length charset) maxl]
    plains = L.map (genPlain charset) foundIndices
  in
    zip plains $ L.map plainToDigest plains
