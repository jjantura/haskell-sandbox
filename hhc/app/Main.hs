{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import System.Directory
import System.Exit
import System.IO
import System.Clock
import Data.List
import Data.Maybe

import Control.Exception
import Control.Monad
import Crypto.Hash             (hashWith, HashAlgorithm(..), SHA1 (..))

import Bruteforce
import Dictionary

import File


-- TODO: this code is ugly and unreadable, replace with applicative optparse asap
takeArgValue :: [String] -> String -> Maybe String
takeArgValue args arg = 
    let maybeArgIndex = arg `elemIndex` args in
    let index = fromJust maybeArgIndex in
    let argsCount = length args in   
        if isJust maybeArgIndex then 
            if index < argsCount then Just (args !! succ index) else Nothing
        else Nothing

asInt :: Maybe String -> Int
asInt s = if isJust s then read $ fromJust s :: Int else -1

asString :: Maybe String -> String
asString s = if isJust s then fromJust s else ""

crack_bf :: [String] -> IO()
crack_bf args = do    
    let maybeAlgorithm = takeArgValue args "-a" 
        charset = asString $ takeArgValue args "-c"
        minLen = asInt $ takeArgValue args "-ll"
        maxLen = asInt $ takeArgValue args "-ul" in
        do 
            content <- loadFile $ asString $ takeArgValue args "-i"             
            print $ bruteforce SHA1 charset minLen maxLen $ lines content

dispatch :: [String] -> IO()
dispatch args = do    
    let incorrectUsage = putStrLn "incorrect usage\n" 
        processing = putStrLn "processing, wait...\n" 
        maybeMode = takeArgValue args "-m" in
            if isJust maybeMode then do
                case fromJust maybeMode of 
                    "bruteforce" -> crack_bf args
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
