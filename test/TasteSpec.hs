module TasteSpec where
import Taste
import Test.Hspec

spec :: Spec
spec = do
    describe "sum a num list" $ do
        it "encodes multiples of 3" $
            sum' [1,2,3] `shouldBe` (6 :: Int)
