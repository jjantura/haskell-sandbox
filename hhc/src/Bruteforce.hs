module Bruteforce

    ( useBruteforce,
      bruteforce
    ) where

import           Common
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
import           System.Exit


argList :: [String] -> [(Maybe String, Maybe String)]
argList args = L.map (takeArgValue args) ["-a", "-c", "-ll", "-ul", "-i"]

useBruteforce :: [String] -> IO()
useBruteforce args =
                if validateArgs al then do
                    content <- loadFile input
                    print $ alg2bf alg charset minLen maxLen $ lines content
                else do
                    putStrLn "Error: incorrect usage. Usage: hhc -m bruteforce -a [MD5|SHA1] -c [charset] -ll [lower length limit] -ul [upper length limit] -i [input_file_with_hashes]"
                    exitFailure
            where
                al = argList args
                am = fromList al
                alg = asString $ am ! Just "-a"
                charset = asString $ am ! Just "-c"
                minLen = asInt $ am ! Just "-ll"
                maxLen = asInt $ am ! Just "-ul"
                input = asString $ am ! Just "-i"



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
