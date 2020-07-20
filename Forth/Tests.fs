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

2VARIABLE BAR

T{ ." two order variables can be exchanged " CR
    0  5 10 MAKE-ORDER FOO ORDER!
    3 10 14 MAKE-ORDER BAR ORDER!
    FOO BAR ORDER-EXCHANGE
    FOO ORDER@
    2DUP START-TIME 3 ?S
    2DUP DURATION  10 ?S
    PRICE          14 ?S
    BAR ORDER@
    2DUP START-TIME 0 ?S
    2DUP DURATION   5 ?S
    PRICE          10 ?S }T

BYE
