module Rent where
import Data.Map (Map, empty, insert, lookupGE)
import Data.List (sortBy, reverse)
import Data.Ord

type Plan = Map Time Money
type Time = Int
type Money = Int
type Order = (Time, Time, Money)

value :: Time -> Plan -> Money
value time plan = case lookupGE time plan of
                    Just (_,v) -> v
                    Nothing    -> 0

profit :: [Order] -> Money
profit = snd . foldl add (empty, 0) . sortBy (flip (comparing start_time))
    where

    start_time (t,_,_) = t

    add (plan, current) (time, duration ,price) = (insert time new plan, new)
        where

        new = max (price + (value (time + duration) plan)) current

solve :: [[Int]] -> [Money]
solve = solveCases . tail
    where

    solveCases v | null v = []
    solveCases v          = solution c : solveCases cs
        where
        (h,rest) = splitAt 1 v
        [n]      = head h
        (c,cs)   = splitAt n rest

    solution = profit . map order

    order [t,d,p] = (t,d,p)

process :: String -> String
process = unlines . map show . solve . map (map read . words) . lines

