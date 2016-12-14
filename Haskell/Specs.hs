import Test.Hspec
import Rent

main = hspec $ do
    describe "Plan" $ do
        it "should retrieve zero if no value for a given time" $ do
            let p = empty
            value 42 p `shouldBe` 0
        it "should retrieve a value that was put in the plan via update" $ do
            let p = update 4807 42 empty
                q = update 2340 17 p
            value 42 q `shouldBe` 4807
            value 17 q `shouldBe` 2340
        it "should update a value only with a greater value" $ do
            let p = update 4807 42 empty
                q = update 4097 42 p
                r = update 5000 42 q
            value 42 q `shouldBe` 4807
            value 42 r `shouldBe` 5000
