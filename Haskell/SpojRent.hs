import Data.Map (Map, empty, insert, lookupGE)
import Data.List (sortBy, reverse)
import Data.Ord
import Data.Vector (Vector, null, drop, head, toList, fromList, tail)
import qualified Data.Vector as V 

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

solve :: Vector [Int] -> [Money]
solve = solveCases . V.tail
    where

    solveCases v | V.null v = []
    solveCases v            = solution c : solveCases cs
        where
        (h,rest) = V.splitAt 1 v
        [n]        = V.head h
        (c,cs)   = V.splitAt n rest

    solution = profit . toList . V.map order

    order [t,d,p] = (t,d,p)

process :: String -> String
process = unlines . Prelude.map show . solve . fromList . Prelude.map (Prelude.map read . words) . lines

main = interact process
