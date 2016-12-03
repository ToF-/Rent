module Rent where
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Map (Map, empty, insertWith, findWithDefault)

type Order    = (Time, Duration, Money)
type Time     = Int
type Duration = Int
type Money    = Int
type Plan     = Map Time Money

data Action = Cash Time
            | Rent Time Duration Money
    deriving (Eq,Ord,Show)

profit :: [Order] -> Money
profit = fst . foldl perform (0, empty) . sortBy (comparing timeThenCategory) . foldl actions []
    where
    perform :: (Money,Plan) -> Action -> (Money,Plan)
    perform (profit,plan) (Cash time)                = (max profit (findWithDefault 0 time plan), plan)
    perform (profit,plan) (Rent time duration price) = (profit, insertWith max (time+duration) (profit+price) plan)

    actions :: [Action] -> Order -> [Action]
    actions l (time,duration,price) = (Cash (time+duration) : Rent time duration price : l)

    timeThenCategory :: Action -> (Time,Time)
    timeThenCategory (Cash t)     = (t,0)
    timeThenCategory (Rent t _ _) = (t,1)
