import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do

    describe "SHA1.bruteforce" $ do
        it "cracks one letter password for range [1-1]" $ do
            sha1bf "asdfghjkl" 1 1 "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8" `shouldBe` ("86f7e437faa5a7fce15d1ddcb9eaeaea377667b8", "a")

        it "cracks one letter password for range [1-2]" $ do
            sha1bf "asdfghjkl" 1 2 "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8" `shouldBe` ("86f7e437faa5a7fce15d1ddcb9eaeaea377667b8", "a")

        it "cracks two letters password for range [1-1]" $ do
            sha1bf "asdfghjkl" 1 1 "e0c9035898dd52fc65c41454cec9c4d2611bfb37" `shouldBe` ("e0c9035898dd52fc65c41454cec9c4d2611bfb37", "")

        it "cracks two letters password for range [1-2]" $ do
            sha1bf "asdfghjkl" 1 2 "e0c9035898dd52fc65c41454cec9c4d2611bfb37" `shouldBe` ("e0c9035898dd52fc65c41454cec9c4d2611bfb37", "aa")

        it "returns empty string for _invalid_ charset" $ do
            sha1bf "123456" 1 1 "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8" `shouldBe` ("86f7e437faa5a7fce15d1ddcb9eaeaea377667b8", "")

