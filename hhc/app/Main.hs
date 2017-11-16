{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Char
import           Data.List          as L
import           Data.Map           as M
import           Data.Maybe
import           Data.Tuple.Select
import           System.Clock
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO

import           Control.Exception
import           Control.Monad
import           Crypto.Hash        (HashAlgorithm (..), MD5 (..), SHA1 (..),
                                     hashWith)

import           Benchmark
import           Bruteforce
import           Charset
import           Dictionary

import           CommandLine
import           File



dispatch :: [String] -> IO()
dispatch args =
    let incorrectUsage = putStrLn "incorrect usage, use -m switch \n"
        processing = putStrLn "processing, wait...\n"
        maybeMode = snd $ takeArgValue args "-m" in
            if isJust maybeMode then do
                case fromJust maybeMode of
                    "benchmark"  -> benchmark
                    "bruteforce" -> useBruteforce args
                    "dictionary" -> useDictionary args
                    "rules"      -> putStrLn "Not Implemented Yet"
                    _            -> putStrLn "Unknown mode"
                exitSuccess
            else do
                incorrectUsage
                exitFailure

main :: IO ()
main = do
    args <- getArgs
    dispatch args
