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

import Control.Exception
import Control.Monad
import Crypto.Hash             (hashWith, HashAlgorithm(..), SHA1 (..), MD5 (..))

import Benchmark
import Charset
import Bruteforce
import Dictionary

import File

-- TODO: this code is ugly and unreadable, replace with applicative optparse asap
takeArgValue :: [String] -> String -> (Maybe String, Maybe String)
takeArgValue args arg = 
    let maybeArgIndex = arg `elemIndex` args in
    let index = fromJust maybeArgIndex in
    let argsCount = length args in   
        if isJust maybeArgIndex then 
            if index < argsCount then (Just arg, Just (args !! succ index)) else (Just arg, Nothing)
        else (Nothing, Nothing)

asInt :: Maybe String -> Int
asInt s = if isJust s then read $ fromJust s :: Int else -1

asString :: Maybe String -> String
asString s = if isJust s then fromJust s else ""

-- TODO: fix string to hash alg conversion
alg2bf :: String -> String -> Int -> Int -> [String] -> [(String, String)]
alg2bf alg = 
    let lowercased = L.map toLower alg in        
        case lowercased of
            "sha1" -> bruteforce SHA1
            _ -> bruteforce MD5

argMap :: [String] -> Map (Maybe String) (Maybe String)
argMap args = M.fromList $ L.map (takeArgValue args) ["-a", "-c", "-ll", "-ul", "-i"]

userBruteforce :: [String] -> IO()
userBruteforce args = do            
        let am = argMap args
            alg = asString $ am ! Just "-a"
            charset = asString $ am ! Just "-c"
            minLen = asInt $ am ! Just "-ll"
            maxLen = asInt $ am ! Just "-ul" 
            input = asString $ am ! Just "-i" in
            do 
                content <- loadFile input             
                print $ alg2bf alg charset minLen maxLen $ lines content


dispatch :: [String] -> IO()
dispatch args = do    
    let incorrectUsage = putStrLn "incorrect usage, use -m switch \n" 
        processing = putStrLn "processing, wait...\n" 
        maybeMode = snd $ takeArgValue args "-m" in
            if isJust maybeMode then do
                case fromJust maybeMode of 
                    "benchmark" -> benchmark
                    "bruteforce" -> userBruteforce args
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
