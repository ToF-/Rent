-- Main.hs
module Main where
import Rent
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

main = BS.interact process

