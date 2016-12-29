\ Tests.fs  Tests for Rent

REQUIRE ffl/tst.fs  \  Forth Foundation Library  Testing definitions
REQUIRE Rent.fs     

T{

." an order can be encoded and decoded to and from a cell" CR
    0 5 100 ORDER>CELL
    3 7 140 ORDER>CELL
    SWAP
    CELL>ORDER 100 ?S 5 ?S 0 ?S
    CELL>ORDER 140 ?S 7 ?S 3 ?S

." encoded orders can be compared on start time" CR
    5 0 0   ORDER>CELL
    0 5 100 ORDER>CELL 
    > ?TRUE
    3 0 0   ORDER>CELL
    3 7 140 ORDER>CELL
    <= ?TRUE
}T
BYE

