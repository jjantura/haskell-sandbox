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
    describe "SHA1.dictionary" $ 
        it "cracks one letter SHA1 password with dictionary" $
            dictionary SHA1 ["a"] ["86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"] `shouldBe` [("a", "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8")]