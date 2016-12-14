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
