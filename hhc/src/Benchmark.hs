module Benchmark (
    benchmark
) where

import System.Clock
import Bruteforce
import Charset
import Crypto.Hash             (hashWith, HashAlgorithm(..), SHA1 (..), MD5 (..))

benchmark :: IO()                
benchmark = do
    putStrLn "--= start =--"    
    putStrLn "bruteforce MD5: one plaintext [4], plaintext len 1-4, charset: all printable ASCII, single core"
    start <- getTime Monotonic
    let result = bruteforce MD5 allPrintableASCIICharset 1 4 ["e5a4601548b3e753eb6a6a484af87c03"]
    putStrLn $ show $ result
    end <- getTime Monotonic
    let timeInMillis = (fromIntegral $ toNanoSecs $ diffTimeSpec start end) / 1000000
    putStrLn $ (show $ (((fromIntegral $ (keySpace (length allPrintableASCIICharset) 1 4)) / timeInMillis * 1000))) ++ " c/s"
    putStrLn "--= stop =--"    
    