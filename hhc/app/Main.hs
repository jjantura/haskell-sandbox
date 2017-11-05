{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import System.Directory
import System.Exit
import System.IO
import System.Clock
import Data.Char
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Tuple.Select

import Control.Exception
import Control.Monad
import Crypto.Hash             (hashWith, HashAlgorithm(..), SHA1 (..), MD5 (..))

import Benchmark
import Charset
import Bruteforce
import Dictionary

import File
import CommandLine



dispatch :: [String] -> IO()
dispatch args =     
    let incorrectUsage = putStrLn "incorrect usage, use -m switch \n" 
        processing = putStrLn "processing, wait...\n" 
        maybeMode = snd $ takeArgValue args "-m" in
            if isJust maybeMode then do
                case fromJust maybeMode of 
                    "benchmark" -> benchmark
                    "bruteforce" -> useBruteforce args
                    "dictionary" -> putStrLn "Not Implemented Yet"
                    "rules" -> putStrLn "Not Implemented Yet"
                    _ -> putStrLn "Unknown mode"
                exitSuccess                       
            else do
                incorrectUsage
                exitFailure
    
main :: IO ()
main = do
    args <- getArgs
    dispatch args
