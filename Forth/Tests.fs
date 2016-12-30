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
    #ORDERS @ 0 ?S
    @NEXT-ORDER @ ORDERS ?S 

." adding an order stores that order and update next order position" CR
    INITIALIZE
    0 5 100 ADD-ORDER
    ORDERS @ DECODE-ORDER 100 ?S 5 ?S 0 ?S
    @NEXT-ORDER @ ORDERS CELL+ ?S
    #ORDERS @ 1 ?S

." adding an order is not allowed if there is already 10000 orders in the array" CR
    10000 #ORDERS !
    3 7 140 ADD-ORDER 
    #ORDERS @ 10000 ?S

." finding the nearest order to a given time or the maximum order" CR
    INITIALIZE
    0 5 100 ADD-ORDER
    3 7 140 ADD-ORDER
    5 9  80 ADD-ORDER
    6 9  70 ADD-ORDER
    2000000 0 0 ADD-ORDER
    5 ORDERS NEAREST-ORDER @ DECODE-ORDER 80 ?S 9 ?S 5 ?S
    10 ORDERS CELL+ NEAREST-ORDER @ DECODE-ORDER 0 ?S 0 ?S 2000000 ?S
}T
BYE

