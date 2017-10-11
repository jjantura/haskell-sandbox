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


-- TODO: this code is ugly and unreadable, replace with applicative optparse asap
takeArgValue :: String -> [String] -> Maybe String
takeArgValue arg args = 
    let maybeArgIndex = arg `elemIndex` args in
    let index = fromJust maybeArgIndex in
    let argsCount = length args in   
        if isJust maybeArgIndex then 
            if index < argsCount then Just (args !! succ index) else Nothing
        else Nothing


dispatch :: [String] -> IO()
dispatch args = do    
    let incorrectUsage = putStrLn "incorrect usage\n" 
        processing = putStrLn "processing, wait...\n" 
        maybeMode = takeArgValue "-m" args in
            if isJust maybeMode then do
                case fromJust maybeMode of 
                    "bruteforce" -> putStrLn "Not Implemented Yet"
                    "dictionary" -> putStrLn "Not Implemented Yet"
                    "rules" -> putStrLn "Not Implemented Yet"
                    "benchmark" -> putStrLn "Not Implemented Yet"
                    _ -> putStrLn "Unknown mode"
                exitSuccess                       
            else do
                incorrectUsage
                exitFailure
       
    
main :: IO ()
main = do
    args <- getArgs
    dispatch args
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
