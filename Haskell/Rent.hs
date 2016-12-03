module Rent where
import Data.List
import Data.Ord
import Data.Map (Map, empty, insertWith, findWithDefault)

type Order    = (Time, Duration, Money)
type Time     = Int
type Duration = Int
type Money    = Int
type Plan     = Map Time Money

data Action = Cash Time
            | Rent Time Duration Money
    deriving (Eq,Ord,Show)

actions :: [Order] -> [Action]
actions = sortBy (comparing timeAndCategory) . foldl actions []
    where 
    actions l (time,duration,price) = (Cash (time+duration) : Rent time duration price : l)
    timeAndCategory (Cash t)     = (t,0)
    timeAndCategory (Rent t _ _) = (t,1)    

compute :: (Money,Plan) -> Action -> (Money,Plan)
compute (profit,plan) (Cash time)                = (max profit (findWithDefault 0 time plan), plan)
compute (profit,plan) (Rent time duration price) = (profit, insertWith max (time+duration) (profit+price) plan)

profit :: [Order] -> Money
profit = fst . foldl compute (0, empty) . actions
