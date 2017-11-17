module Benchmark (
    benchmark
) where

import           Bruteforce
import           Charset         as Cset
import           Control.DeepSeq
import           Crypto.Hash     (HashAlgorithm (..), MD5 (..), SHA1 (..),
                                  hashWith)
import           System.Clock

headers :: [String]
headers = ["method", "algorithm", "hashes count", "keyspace", "thread count", "performance[c/s]", "result"]

benchmark :: IO()
benchmark = do
    putStrLn $ unwords $ map (++ ", ") headers
    start <- getTime Monotonic
    let result = bruteforce MD5 Cset.allPrintableASCIICharset 1 3 ["b55e74d4007b674b329d70f5550028ba"]
    end <- result `deepseq` getTime Monotonic
    let timeInMillis = fromIntegral (toNanoSecs $ diffTimeSpec start end) / 1000000
    let keySpace = Cset.keySpace (length Cset.allPrintableASCIICharset) 1 3
    let performance = fromIntegral keySpace / timeInMillis * 1000
    putStrLn $ "bruteforce, MD5, 1, " ++ show keySpace ++ ", 1, " ++ show performance

