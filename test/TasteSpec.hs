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


    describe "num operation" $ do
      it "double a num" $
        double 3 `shouldBe` (6 :: Int)
      it "quadruple a num" $
        quadruple 3 `shouldBe` (12 :: Int)
      it "factorial a num" $
        factorial 10 `shouldBe` (3628800 :: Int)
      it "average a int list" $
        average [1,2,3,4,5] `shouldBe` (3.0 :: Float)
      it "average a float list" $
        average [1.0,2.0,3.0,4.0,5.0] `shouldBe` (3.0 :: Float)

    describe "list operation" $ do
      it "get head of a list" $
        head [1,2,3,4,5] `shouldBe` (1 :: Int)
      it "get tail of a list" $
        tail [1,2,3,4,5] `shouldBe` ([2,3,4,5] :: [Int])
      it "get nth element of a list" $
        [1,2,3,4,5] !! 2 `shouldBe` (3 :: Int)
      it "select first n elements of a list" $
        take 3 [1,2,3,4,5] `shouldBe` ([1,2,3] :: [Int])
      it "remove first n elements of a list" $
        drop 3 [1,2,3,4,5] `shouldBe` ([4,5] :: [Int])
      it "length of a list" $
        length ([1,2,3,4,5] :: [Int]) `shouldBe` (5 :: Int)
      it "sum of a list" $
        sum [1,2,3,4,5] `shouldBe` (15 :: Int)
      it "product of a list" $
        product [1,2,3,4,5] `shouldBe` (120 :: Int)
      it "append two lists" $
        [1,2,3] ++ [4,5] `shouldBe` ([1,2,3,4,5] :: [Int])
      it "reverse a list" $
        reverse [1,2,3,4,5] `shouldBe` ([5,4,3,2,1] :: [Int])

    describe "layout rule" $ do
      it "use implicit syntax" $
        implicit * 2 `shouldBe` (6 :: Int)
      it "use explicit syntax" $
        explicit * 2 `shouldBe` (6 :: Int)
