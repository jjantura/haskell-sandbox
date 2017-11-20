module RulesSpec where
    
import           Rules
import           Test.Hspec
import           Test.QuickCheck
    
spec :: Spec
spec = 
    describe "rules" $ do
        it "duplicates string" $ _duplicate "Cat" `shouldBe` "CatCat"
        it "reverses string" $ _reverse "Cat" `shouldBe` "taC"