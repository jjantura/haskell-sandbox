module Main where

import System.Environment
import System.Directory
import System.Exit
import System.IO

import Control.Monad

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
  -- TODO: move to method (?), introduce context type (?)
    args <- getArgs
    validateArgs args
    let charset = args !! 0
    let minLen = read $ args !! 1 :: Int
    let maxLen = read $ args !! 2 :: Int
    let path = args !! 3
    -- read file
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    let hashes = lines contents
    let plains = map (sha1bf charset minLen maxLen) hashes
    --let results = map (sha1bf') hashes
    putStrLn $ plains !! 0
