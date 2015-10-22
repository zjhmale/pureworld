module TasteSpec where
import Taste
import Test.Hspec

spec :: Spec
spec = do
    describe "sum a num list" $ do
      it "encodes multiples of 3" $
        sum' [1,2,3] `shouldBe` (6 :: Int)

    describe "quick sort a num list" $ do
      it "quick sort [1,5,4,2,3]" $
        qsort [1,5,4,2,3] `shouldBe` ([1,2,3,4,5] :: [Int])
