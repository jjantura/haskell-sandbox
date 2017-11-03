{-# LANGUAGE TupleSections #-}
module DictionarySpec where 

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Dictionary
import Charset
import Crypto.Hash (hashWith, SHA1 (..), MD5 (..))

spec :: Spec
spec = 
    describe "SHA1.dictionary" $ do
        it "cracks one letter SHA1 password with dictionary" $
            dictionary SHA1 ["a"] ["86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"] `shouldBe` [("a", "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8")]
        it "cracks seven letter SHA1 password with dictionary" $
            dictionary SHA1 ["aaaa", "1234567"] ["20eabe5d64b0e216796e834f52d61fd0b70332fc"] `shouldBe` [("1234567", "20eabe5d64b0e216796e834f52d61fd0b70332fc")]