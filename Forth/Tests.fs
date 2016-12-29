\ Tests.fs  Tests for Rent

REQUIRE ffl/tst.fs  \  Forth Foundation Library  Testing definitions
REQUIRE Rent.fs     

T{

." an order can be encoded and decoded to and from a cell" CR
    0 5 100 ENCODE-ORDER
    3 7 140 ENCODE-ORDER
    SWAP
    DECODE-ORDER 100 ?S 5 ?S 0 ?S
    DECODE-ORDER 140 ?S 7 ?S 3 ?S

." encoded orders can be compared on start time" CR
    5 0 0   ENCODE-ORDER
    0 5 100 ENCODE-ORDER 
    > ?TRUE
    3 0 0   ENCODE-ORDER
    3 7 140 ENCODE-ORDER
    <= ?TRUE

." after initialization, there is no order in the array" CR
    INITIALIZE
    LAST-ORDER @ ORDERS ?S 
}T
BYE

