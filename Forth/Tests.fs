    REQUIRE ffl/tst.fs
    REQUIRE Rent.fs

    T{
    ." PLAN value (eg at 42) is O by default" CR
    42 PLAN [@|0]  ?0

    ." after updating the value can be retrieved" CR
    4807 42 PLAN [>!]
    42 PLAN [@|0]  4807 ?S

    ." updating with a smaller value is not possible" CR
    4807 42 PLAN [>!]
    4096 42 PLAN [>!]
    42 PLAN [@|0]  4807 ?S

    ." when updating cash, profit should increase" CR
    PROFIT @ ?0
    4807 42 PLAN [>!]
    42 CASH
    PROFIT @ 4807 ?S

    ." when updating cash, profit should not decrease" CR
    4807 42 PLAN [>!]
    4096 43 PLAN [>!]
    42 CASH
    43 CASH
    PROFIT @ 4807 ?S

    ." planning rent t d p update plan at t+d with profit + p" CR
    500 PROFIT !
    500 10 PLAN [>!]
    10 7 450  RENT
    17 PLAN [@|0]  950 ?S

    ." maximize profit from cash and rent operations" CR
    PLAN 200 ERASE 0 PROFIT !
    0 5 100 RENT
    3 7 140 RENT
    5       CASH
    5 9 80  RENT
    6 9 70  RENT
    10      CASH
    14      CASH
    15      CASH
    PROFIT @  180 ?S
    
    }T
    BYE
