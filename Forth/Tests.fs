    REQUIRE ffl/tst.fs
    REQUIRE Rent.fs

    T{
    ." 
    ." values can be stored and retrieved in the plan table" CR
    4807 42 PLAN!  42 PLAN@ 4807 ?S
    256  17 PLAN!  17 PLAN@ 256  ?S

    ." value can be stored only if greater than value present" CR
    500  23 PLAN!  23 PLAN@  500 ?S
    250  23 PLAN!  23 PLAN@  500 ?S  

    ." initialize set profit and plan to zeroes" CR
    4807 PROFIT !
    100 42 PLAN!
    INITIALIZE
    PROFIT @ ?0
    42 PLAN@ ?0

    ." when updating cash, profit should increase" CR
    INITIALIZE
    PROFIT @ ?0
    4807 42 PLAN!
    42 CASH
    PROFIT @ 4807 ?S

    ." when updating cash, profit should not decrease" CR
    INITIALIZE
    4807 42 PLAN!
    4096 43 PLAN!
    42 CASH
    43 CASH
    PROFIT @ 4807 ?S

    ." planning rent t d p update plan at t+d with profit + p" CR
    INITIALIZE
    500 10 PLAN!
    10 7 450  RENT
    17 PLAN@  950 ?S

    ." maximize profit from cash and rent operations" CR
    INITIALIZE
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
