module Main where

import System.Environment
import System.Directory
import System.IO
import Lib

main :: IO ()
main = do
    args <- getArgs
    print $ sha1bf ['a'..'z'] 1 5 "b40981aab75932c5b2f555f50769d878e44913d7"
