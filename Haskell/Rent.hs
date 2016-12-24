module Rent where
import Data.Map (Map, empty, insert, lookupGE, findMin)
import Data.Maybe
import Data.List (sortBy, reverse)
import Data.Ord
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

type Plan = Map Time Money
type Time = Int
type Money = Int
type Order = (Time, Time, Money)

maxTime = 10000000

initial :: Plan
initial = insert maxTime 0 empty

value :: Time -> Plan -> Money
value time plan = snd $ fromJust (lookupGE time plan)

add :: (Plan,Money) -> Order -> (Plan,Money)
add (plan,last) (time, duration ,price) = (insert time best plan, best)
    where
    best = max (price + (value (time + duration) plan)) last

profit :: [Order] -> Money
profit = value 0 . fst . foldl add (initial,0) . sortBy (flip (comparing time))
    where
    time (t,_,_) = t

solve :: [[Int]] -> [Money]
solve = solveCases . tail
    where
    solveCases [] = []
    solveCases ([n]:os) = solution (take n os) : solveCases (drop n os)
    solution = profit . map order

    order [t,d,p] = (t,d,p)

process :: ByteString -> ByteString
process = BS.unlines . map (BS.pack . show) . solve . map (map (read . BS.unpack) . BS.words) . BS.lines

