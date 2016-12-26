import Test.Hspec
import Rent
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Vector

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
    describe "solve" $ do
        it "should solve several cases, and ignore the first line of input" $ do
            solve [[2],[1],[0, 5, 100],[2],[0, 5, 120],[3, 7, 140]] `shouldBe` [100, 140]
    describe "process" $ do
        it "should process the input string and produce and output string" $ do
            process ("2\n1\n0 5 100\n2\n0 5 12\n3 7 140\n") `shouldBe` "100\n140\n"

