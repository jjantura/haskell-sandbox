module Main where

import System.Environment
import System.Directory
import System.Exit
import System.IO
import Lib

validateArgs :: [String] -> IO ()
validateArgs args =
    let incorrectUsage = putStrLn "incorrect usage\nusage charset minLen maxLen file_with_hashes" in
    let processing = putStrLn "processing, wait...\n" in
    let argsLen = length args in
      if (argsLen < 4 || argsLen > 4) then do
        incorrectUsage
        exitFailure
      else do
          processing


main :: IO ()
main = do
    args <- getArgs
    validateArgs args
    let charset = args !! 0
    let minLen = read $ args !! 1 :: Int
    let maxLen = read $ args !! 2 :: Int
    print $ sha1bf charset minLen maxLen "b40981aab75932c5b2f555f50769d878e44913d7"
