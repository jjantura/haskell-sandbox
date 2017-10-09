{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import System.Directory
import System.Exit
import System.IO
import System.Clock

import Formatting
import Formatting.Clock

import Control.Exception
import Control.Monad
import           Crypto.Hash             (hashWith, SHA1 (..))
import Options.Applicative
import Data.Semigroup ((<>))

import Bruteforce

-- data ProgramArgs = ProgramArgs {
--     mode :: String
-- }

-- programArgs :: Parser ProgramArgs
-- programArgs = ProgramArgs 
--                 <$> strOption
--                 ( 
--                     long "mode"
--                     <> short 'm'
--                     <> metavar "MODE"
--                     <> help "any of available modes [benchmark, bruteforce, dict, rules]"
--                 )

validateArgs :: [String] -> IO ()
validateArgs args =
    let incorrectUsage = putStrLn "incorrect usage\nusage charset minLen maxLen file_with_hashes" in
    let processing = putStrLn "processing, wait...\n" in
    let argsLen = length args in
      if argsLen < 4 || argsLen > 4 then do
        incorrectUsage
        exitFailure
      else processing


-- dispatch :: ProgramArgs -> IO ()
-- dispatch (ProgramArgs m) = putStrLn $ "Selected mode: " ++ m
-- dispatch  _ = return ()      

main :: IO ()
main = do
--  TODO: move to method (?), introduce context type (?)
    hSetEncoding stdout utf8
    args <- getArgs
    validateArgs args
    let charset = head args
        minLen = read $ args !! 1 :: Int
        maxLen = read $ args !! 2 :: Int
        path = args !! 3
    -- read file
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    time0 <- getTime Monotonic

    let hashes = lines contents
        plains = bruteforce SHA1 charset minLen maxLen hashes

    print plains
    time1 <- getTime Monotonic

    let timeDiff = time1 - time0
    fprint (timeSpecs % "\n") time0 time1
