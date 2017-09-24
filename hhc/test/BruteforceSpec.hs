{-# LANGUAGE TupleSections #-}
module BruteforceSpec where 

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Bruteforce
import Charset
import           Crypto.Hash             (hashWith, SHA1 (..), MD5 (..))

spec :: Spec
spec = do

    describe "SHA1.bruteforce" $ do
        it "cracks one letter SHA1 password for range [1-1]" $
            bruteforce SHA1 "asdfghjkl" 1 1 ["86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"] `shouldBe` [("a", "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8")]

        it "cracks one letter SHA1 password for range [1-2]" $
            bruteforce SHA1 "asdfghjkl" 1 2 ["86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"] `shouldBe` [("a", "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8" )]

        it "cracks two letters SHA1 password for range [1-1]" $
            bruteforce SHA1 "asdfghjkl" 1 1 ["e0c9035898dd52fc65c41454cec9c4d2611bfb37"] `shouldBe` []

        it "cracks two letters SHA1  password for range [1-2]" $
            bruteforce SHA1 "asdfghjkl" 1 2 ["e0c9035898dd52fc65c41454cec9c4d2611bfb37"] `shouldBe` [("aa", "e0c9035898dd52fc65c41454cec9c4d2611bfb37")]

        it "returns empty string for _invalid_ charset" $
            bruteforce SHA1 "123456" 1 1 ["86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"] `shouldBe` []
    
    describe "MD5.bruteforce" $ do
        it "cracks one letter MD5 password for range [1-1]" $
            bruteforce MD5 "asdfghjkl" 1 1 ["0cc175b9c0f1b6a831c399e269772661"] `shouldBe` [("a", "0cc175b9c0f1b6a831c399e269772661")]

        it "cracks one letter MD5 password for range [1-2]" $
            bruteforce MD5 "asdfghjkl" 1 2 ["0cc175b9c0f1b6a831c399e269772661"] `shouldBe` [("a", "0cc175b9c0f1b6a831c399e269772661" )]

        it "cracks two letters MD5 password for range [1-1]" $
            bruteforce MD5 "asdfghjkl" 1 1 ["4124bc0a9335c27f086f24ba207a4912"] `shouldBe` []

        it "cracks two letters MD5 password for range [1-2]" $
            bruteforce MD5 "asdfghjkl" 1 2 ["4124bc0a9335c27f086f24ba207a4912"] `shouldBe` [("aa", "4124bc0a9335c27f086f24ba207a4912")]

