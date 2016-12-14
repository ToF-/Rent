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
update v t p = (t,max v (value t p)):p

data Action = Cash Time | Rent Time Time Money
            
perform :: (Money, Plan) -> Action -> (Money, Plan)
perform (profit,plan) (Rent t d p) = (profit, update (profit+p) (t+d) plan)
perform (profit,plan) (Cash t)     = (max profit (value t plan), plan)
