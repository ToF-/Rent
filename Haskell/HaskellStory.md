Designing a solution in Haskell to the Rent problem involves tackling these problems:

- updating and getting values from a *plan*, which is a mapping from time to money
- applying actions (rent, cash) that change the current profit value and the current plan
- translating the values given as input into actions
- reading several cases on the input stream in an efficient way 

Updating and getting values from a plan
---------------------------------------

 What is a *Plan*? It is simply a data structure that should allow for retrieve and update, and follow those two rules:

- if a value for a given time is not in the plan, then the value is zero
- an update for a given time is possible if the new value is greater than the current value  

Let's write our first test:

    import Test.Hspec
    import Rent

    main = hspec $ do
        describe "Plan" $ do
            it "should retrieve zero if no value for a given time" $ do
                let p = empty
                value 42 p  `shouldBe` 0

It is very easy to make this test pass:

    module Rent where

    empty = []

    value _ _ = 0

Let's immediately write another test:

    it "should retrieve a value that was put in the plan via update" $ do
        let p = update 4807 42 empty
            q = update 2340 17 p
        value 42 q `shouldBe` 4807
        value 17 q `shouldBe` 2340

Now we have to add a bit more functionality. Using the standard `lookup` for retrieve, we can define our Plan to be a list of (time,money) pairs. 

    module Rent where

    empty = []

    value t p = case lookup t p of
        Just v  -> v
        Nothing -> 0

    update v t p = (t,v):p

We can refactor for documentation, adding types declarations and signatures:

    module Rent where

    type Plan = [(Time, Money)]
    type Time = Int
    type Money = Int

    empty :: Plan
    empty = []

    value :: Time -> Plan -> Money
    value t p = case lookup t p of
        Just v  -> v
        Nothing -> 0

    update :: Money -> Time -> Plan -> Plan
    update v t p = (t,v):p

Let's implement our update rule:

    it "should update a value only with a greater value" $ do
        let p = update 4807 42 empty
            q = update 4097 42 p
            r = update 5000 42 q
        value 42 q `shouldBe` 4807
        value 42 r `shouldBe` 5000

Using max with the current value is a possible way to implement the rule:

    update :: Money -> Time -> Plan -> Plan
    update v t p = (t,max v (value t p)):p

Applying actions that change profit and plan
--------------------------------------------

There are two types of actions:

- Cash at a given time: establishing the profit at that time, taking the plan into account 
- Rent: updating the plan at the given time + duration, with the value defined by profit + the given price

We start with a test on the Rent action. When we perform a Rent action, the profit is unchanged, but the value stored in the plan at the end of the rent should be set:  

    describe "Action" $ do 
        it "if a Rent, should update the plan" $ do
            let initial = (800, empty)
                (profit,plan) = perform initial (Rent 10 32 4007)   
            profit `shouldBe` 800
            value 42 plan `shouldBe` 4807

The perform function does this:

    data Action = Rent Time Time Money

    perform :: (Money, Plan) -> Action -> (Money, Plan)
    perform (profit,plan) (Rent t d p) = (profit, update (profit+p) (t+d) plan)

A Cash action does the opposite of a Rent action: it changes the profit, but not the plan:

    it "if a Cash, should update profit" $ do
        let initial = (800, update 4807 42 empty)
            (profit,plan) = perform initial (Cash 42)   
        profit `shouldBe` 4807

Let's add the Cash action type, and the corresponding pattern in our perform function:

    data Action = Cash Time | Rent Time Time Money
                
    perform :: (Money, Plan) -> Action -> (Money, Plan)
    perform (profit,plan) (Rent t d p) = (profit, update (profit+p) (t+d) plan)
    perform (profit,plan) (Cash t)     = (max profit (value t plan), plan)

Now we can apply this function to a correctly ordered sequence of actions:

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

Converting Input Values into Actions
------------------------------------

Each order in the input stream is defined by 3 integers value representing the *start time*, *duration* and *price* of the order, and must be converted into two actions that will be added to the list of actions for a given case.

    describe "Case" $ do
        it "should be filled with 2 actions for each order in the case" $ do
            addActions [0, 5, 100] [] `shouldBe` [Cash 5, Rent 0 5 100]                         

To make this test pass, we need to make the `Action` type an instance of `Eq` and `Show`, and then write the  `addActions` function:

    data Action = Cash Time | Rent Time Time Money
        deriving (Eq,Show)

    addActions :: [Int] -> [Action] -> [Action]
    addActions [t,d,p] as = Cash (t+d) : Rent t d p : as

Then we need a function that will convert all the orders in a list into a sorted list of actions:

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

Now the `Action` type also need to be an instance of the class `Ord`:

    data Action = Cash Time | Rent Time Time Money
        deriving (Eq, Show, Ord)

Actions in the list should be sorted by time then category of action, i.e. for a same given time, the cash action should precede any rent action. We get the list of actions by `fold`ing our `addActions` function of the list of orders, and order then sorting this list on a criterion based on a pair of `Int`s representing the time and category of the action:

    actions :: [[Int]] -> [Action]
    actions = sortBy (comparing timeAndCategory) . foldr addActions []
        where
        timeAndCategory :: Action -> (Int, Int)
        timeAndCategory (Cash t)     = (t, 0)
        timeAndCategory (Rent t _ _) = (t, 1) 
