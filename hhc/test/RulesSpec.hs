module RulesSpec where
    
import           Rules
import           Test.Hspec
import           Test.QuickCheck
    
spec :: Spec
spec = do
    describe "rules" $ do
        it "duplicates string" $ _duplicate "Cat" `shouldBe` "CatCat"
        it "reverses string" $ _reverse "Cat" `shouldBe` "taC"
        it "capitalizes string" $ _capitalize "cat" `shouldBe` "CAT"
        it "lowercases string" $ _lowercase "CAT" `shouldBe` "cat"