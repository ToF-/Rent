import Test.Hspec
import Rent
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map (empty)

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
    describe "Action" $ do 
        it "if a Rent, should update the plan" $ do
            let initial = (800, empty)
                (profit,plan) = perform initial (Rent 10 32 4007)   
            profit `shouldBe` 800
            value 42 plan `shouldBe` 4807
        it "if a Cash, should update profit" $ do
            let initial = (800, update 4807 42 empty)
                (profit,plan) = perform initial (Cash 42)   
            profit `shouldBe` 4807
        it "if performed in sequence, should calculate best profit" $ do
            let actions = [Rent 0 5 100
                          ,Rent 3 7 140
                          ,Cash 5
                          ,Rent 5 9 80
                          ,Rent 6 9 70
                          ,Cash 10
                          ,Cash 14
                          ,Cash 15]
                (profit,plan) = foldl perform (0, empty) actions
            profit `shouldBe` 180
    describe "Case" $ do
        it "should be filled with 2 actions for each order in the case" $ do
            addActions [0, 5, 100] [] `shouldBe` [Cash 5, Rent 0 5 100]                         
        it "should be contain all the sorted actions for a case" $ do
            actions [[0, 5, 100],[3, 7, 140],[5, 9, 80],[6, 9, 70]] `shouldBe` 
                 [Rent 0 5 100
                 ,Rent 3 7 140
                 ,Cash 5
                 ,Rent 5 9 80
                 ,Rent 6 9 70
                 ,Cash 10
                 ,Cash 14
                 ,Cash 15]
    describe "profit" $ do
        it "should compute the profit for a list of orders" $ do
            profit [[0, 5, 100],[3, 7, 140],[5, 9, 80],[6, 9, 70]] `shouldBe` 180
    describe "solve" $ do
        it "should solve several cases, and ignore the first line of input" $ do
            solve [[2],[1],[0, 5, 100],[2],[0, 5, 120],[3, 7, 140]] `shouldBe` [100, 140]
    describe "process" $ do
        it "should process the input string and produce and output string" $ do
            process (BS.pack "2\n1\n0 5 100\n2\n0 5 12\n3 7 140\n") `shouldBe` (BS.pack "100\n140\n")

