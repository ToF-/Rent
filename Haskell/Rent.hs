module Rent where
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map, empty, insertWith, findWithDefault)

type Plan = Map Time Money
type Time = Int
type Money = Int


value :: Time -> Plan -> Money
value = findWithDefault 0

update :: Money -> Time -> Plan -> Plan
update v t = insertWith max t v  

data Action = Cash Time | Rent Time Time Money
    deriving (Eq, Show, Ord)

addActions :: [Int] -> [Action] -> [Action]
addActions [t,d,p] as = Cash (t+d) : Rent t d p : as

actions :: [[Int]] -> [Action]
actions = sortBy (comparing timeAndCategory) . foldr addActions []
    where
    timeAndCategory :: Action -> (Int, Int)
    timeAndCategory (Cash t)     = (t, 0)
    timeAndCategory (Rent t _ _) = (t, 1) 
            
perform :: (Money, Plan) -> Action -> (Money, Plan)
perform (profit,plan) (Rent t d p) = (profit, update (profit+p) (t+d) plan)
perform (profit,plan) (Cash t)     = (max profit (value t plan), plan)

profit :: [[Int]] -> Money
profit = fst . foldl perform (0,empty) . actions

solve :: [[Int]] -> [Money]
solve = solutions . tail 
    where 
    solutions [] = []
    solutions ([n]:orders) = profit (take n orders):solutions (drop n orders) 

process :: ByteString -> ByteString
process = BS.unlines . map (BS.pack . show) . solve . map (map (read . BS.unpack) . BS.words) . BS.lines 
