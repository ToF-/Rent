import Test.Hspec
import Rent

main = hspec $ do
    describe "plan" $ do
        it "given the maximal time point and no order, should give a value of 0" $ do
            profit [] `shouldBe` 0

        it "given one order was added, at the minimal time point, should give the price of the order" $ do
            profit [(0, 5, 100)] `shouldBe` 100 

        it "given two compatible orders were added, at the mimimal time point, should give sum of prices" $ do
            profit [(6, 9, 70), (0, 5, 100)] `shouldBe` 170 

        it "given two imcompatible orders were added, at the mimimal time point, should give max of prices" $ do
            profit [(3, 7, 140), (0, 5, 100)] `shouldBe` 140 

        it "given a standard case, should give the best profit" $ do
            profit [(6, 9, 70), (5, 9, 80), (3, 7, 140), (0, 5, 100)]
                 `shouldBe` 180
