module Benchmark (
    benchmark
) where

import           Bruteforce
import           Charset      as Cset
import           Crypto.Hash  (HashAlgorithm (..), MD5 (..), SHA1 (..),
                               hashWith)
import           System.Clock

benchmark :: IO()
benchmark = do
    putStrLn "method,algorithm,hashes count,keyspace,thread count,performance,result"
    start <- getTime Monotonic
    let result = bruteforce MD5 Cset.allPrintableASCIICharset 1 3 ["b55e74d4007b674b329d70f5550028ba"]
    print result
    end <- getTime Monotonic
    let timeInMillis = fromIntegral (toNanoSecs $ diffTimeSpec start end) / 1000000
    let keySpace = Cset.keySpace (length Cset.allPrintableASCIICharset) 1 3
    let performance = fromIntegral keySpace / timeInMillis * 1000
    putStrLn $ "bruteforce,MD5,1," ++ show keySpace ++ ",1," ++ show performance ++ " c/s,"

