{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import System.Directory
import System.Exit
import System.IO
import System.Clock
import Data.List
import Data.Maybe

import Formatting
import Formatting.Clock

import Control.Exception
import Control.Monad
import Crypto.Hash             (hashWith, SHA1 (..))

import Bruteforce
import Dictionary

takeArgValue :: String -> [String] -> Maybe String
takeArgValue arg args = 
    let maybeArgIndex = arg `elemIndex` args in
    let index = fromJust maybeArgIndex in
    let argsCount = length args in   
        if isJust maybeArgIndex then 
            if index < argsCount then Just (args !! succ index) else Nothing
        else Nothing

dispatch :: Maybe String -> IO()
dispatch maybeArg = do    
    let incorrectUsage = putStrLn "incorrect usage\n" 
        processing = putStrLn "processing, wait...\n" in
            if isJust maybeArg then do
                processing
                exitSuccess                       
            else do
                incorrectUsage
                exitFailure

        
-- TODO: this code is ugly and unreadable, replace with applicative optparse asap
validateArgs :: [String] -> IO ()
validateArgs args = dispatch $ takeArgValue "-m" args
    
main :: IO ()
main = do
    args <- getArgs
    validateArgs args
--  TODO: move to method (?), introduce context type (?)
    -- hSetEncoding stdout utf8
    -- args <- getArgs
    -- validateArgs args
    -- let charset = head args
    --     minLen = read $ args !! 1 :: Int
    --     maxLen = read $ args !! 2 :: Int
    --     path = args !! 3
    -- -- read file
    -- handle <- openFile path ReadMode
    -- contents <- hGetContents handle
    -- time0 <- getTime Monotonic

    -- let hashes = lines contents
    --     -- plains = bruteforce SHA1 charset minLen maxLen hashes
        
    -- print plains
    -- time1 <- getTime Monotonic

    -- let timeDiff = time1 - time0
    -- fprint (timeSpecs % "\n") time0 time1
