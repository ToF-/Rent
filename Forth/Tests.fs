REQUIRE ffl/tst.fs
REQUIRE Rent.fs

T{ ." an order has a start time, duration and price " CR
    1000 42 4807 MAKE-ORDER
    2DUP START-TIME 1000 ?S
    2DUP DURATION     42 ?S
    PRICE           4807 ?S }T

2VARIABLE FOO

T{ ." an order can be stored and retrieved " CR
    17 23 42 MAKE-ORDER 
    FOO ORDER!
    FOO ORDER@
    2DUP PRICE    42 ?S
    2DUP DURATION 23 ?S
    START-TIME    17 ?S }T

T{ ." an order can add its duration, creating a new order " CR
    42 17 10 MAKE-ORDER
    ADD-DURATION
    2DUP START-TIME 59 ?S
    2DUP DURATION    0 ?S
    PRICE            0 ?S }T

T{ ." an order is compatible with another if its start time >= the other end-time " CR
    5 9 10 MAKE-ORDER
    0 5 10 MAKE-ORDER 
    COMPATIBLE? -1 ?S

    6 8 10 MAKE-ORDER
    0 5 10 MAKE-ORDER 
    COMPATIBLE? -1 ?S

    3 10 14 MAKE-ORDER
    0 5  10 MAKE-ORDER 
    COMPATIBLE? 0 ?S }T

CREATE SOME-ORDERS 
5  9 7  MAKE-ORDER 2,
0  5 10 MAKE-ORDER 2,
6  8 8  MAKE-ORDER 2,
3 10 14 MAKE-ORDER 2,
2000000 0 0 MAKE-ORDER 2,

CREATE SOME-POINTERS 5 CELLS ALLOT
SOME-ORDERS SOME-POINTERS 5 ORDER@-ARRAY

T{ ." orders can be accessed through an array of order pointers " CR
    SOME-POINTERS 1 CELLS + @ ORDER@ START-TIME 0 ?S
    SOME-POINTERS 3 CELLS + @ ORDER@ PRICE     14 ?S
    SOME-POINTERS 0 CELLS + @ ORDER@ DURATION   9 ?S
    SOME-POINTERS 2 CELLS + @ ORDER@ START-TIME 6 ?S }T

T{ ." MID gives the middle adress between two adresses " CR
    SOME-ORDERS DUP 2 CELLS + MID
    SOME-ORDERS 1 CELLS + ?S
    SOME-ORDERS DUP CELL+ MID
    SOME-ORDERS CELL+ ?S
    SOME-ORDERS DUP 3 CELLS + MID
    SOME-ORDERS 2 CELLS + ?S
}T

T{ ." EXCH exchanges the content of two addresses " CR
    SOME-POINTERS SOME-POINTERS 2 CELLS + EXCH
    SOME-POINTERS 0 CELLS + @ ORDER@ DURATION   8 ?S
    SOME-POINTERS 2 CELLS + @ ORDER@ START-TIME 5 ?S }T

T{ ." PARTITION find partitions around pivot " CR
    SOME-POINTERS 3 CELLS OVER + PARTITION

    SOME-POINTERS 1 CELLS + ?S
    SOME-POINTERS 3 CELLS + ?S
    SOME-POINTERS 3 CELLS + ?S
    SOME-POINTERS ?S
    }T

T{ ." SORT sort the order pointer array " CR
    SOME-POINTERS 5 SORT
    SOME-POINTERS 0 CELLS + @ ORDER@ START-TIME 0 ?S
    SOME-POINTERS 1 CELLS + @ ORDER@ PRICE     14 ?S
    SOME-POINTERS 2 CELLS + @ ORDER@ DURATION   9 ?S
    SOME-POINTERS 3 CELLS + @ ORDER@ START-TIME 6 ?S 
    SOME-POINTERS 4 CELLS + @ ORDER@ START-TIME 2000000 ?S }T

T{ ." NEXT-COMPATIBLE search the table for next compatible order " CR
    0 5 10 MAKE-ORDER SOME-POINTERS NEXT-COMPATIBLE
    SOME-POINTERS 2 CELLS + ?S 
    1000 5 10 MAKE-ORDER SOME-POINTERS NEXT-COMPATIBLE
    SOME-POINTERS 4 CELLS + ?S }T

BYE


