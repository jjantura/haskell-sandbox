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
    putStrLn "bruteforce MD5: one plaintext [3], plaintext len 1-3, charset: all printable ASCII, single core"
    start <- getTime Monotonic
    let result = bruteforce MD5 allPrintableASCIICharset 1 3 ["b55e74d4007b674b329d70f5550028ba"]
    print result
    end <- getTime Monotonic
    let timeInMillis = fromIntegral (toNanoSecs $ diffTimeSpec start end) / 1000000
    putStrLn $ show (fromIntegral (keySpace (length allPrintableASCIICharset) 1 3) / timeInMillis * 1000) ++ " c/s"
    putStrLn "--= stop =--"    
    