{-# LANGUAGE TupleSections #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Bruteforce
import           Crypto.Hash             (hashWith, SHA1 (..))

main :: IO ()
main = hspec $ do

    describe "SHA1.bruteforce" $ do
        it "cracks one letter password for range [1-1]" $
            bruteforce SHA1 "asdfghjkl" 1 1 ["86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"] `shouldBe` [("a", "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8")]

        it "cracks one letter password for range [1-2]" $
            bruteforce SHA1 "asdfghjkl" 1 2 ["86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"] `shouldBe` [("a", "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8" )]

        it "cracks two letters password for range [1-1]" $
            bruteforce SHA1 "asdfghjkl" 1 1 ["e0c9035898dd52fc65c41454cec9c4d2611bfb37"] `shouldBe` []

        it "cracks two letters password for range [1-2]" $
            bruteforce SHA1 "asdfghjkl" 1 2 ["e0c9035898dd52fc65c41454cec9c4d2611bfb37"] `shouldBe` [("aa", "e0c9035898dd52fc65c41454cec9c4d2611bfb37")]

        it "returns empty string for _invalid_ charset" $
            bruteforce SHA1"123456" 1 1 ["86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"] `shouldBe` []
