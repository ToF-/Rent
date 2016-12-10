REQUIRE ffl/tst.fs
REQUIRE Rent.fs

T{
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
        0      50000  100 RENT
        30000  70000  140 RENT
        50000             CASH
        50000  90000  80  RENT
        60000  90000  70  RENT
        100000            CASH
        140000            CASH
        150000            CASH
        PROFIT @  180 ?S

        ." an action can be encoded in a key that is ordered" CR
        4807 0000 ACTION>KEY
        4807 1000 ACTION>KEY
        < ?TRUE

        ." a key can be decoded into action time and duration" CR
        4807 42 ACTION>KEY
        KEY>ACTION  42 ?S  4807 ?S 

        ." action data and key for a cash action to be stored are 0 and (t,0)" CR
        4807 {CASH} 
    KEY>ACTION  0  ?S  4807 ?S  
        NIL ?S

        ." action data and key for a rent action to be stored are p and (t,d)" CR
        4807 42 100 {RENT} 
        KEY>ACTION  42  ?S  4807 ?S  
        100 ?S

        ." adding an order stores a rent action at (t,d) and a cash action at (t+d,0)" CR
        INIT-ACTIONS
        4807 42 100 ADD-ORDER
        4807 42 ACTION>KEY ACTIONS ACT-HAS? ?TRUE
        4849 0  ACTION>KEY ACTIONS ACT-HAS? ?TRUE 

        ." inserting an order with same time and duration is allowed only for a better price" CR
        INIT-ACTIONS
        4807 42 100 ADD-ORDER
        4807 42 ACTION>KEY ACTIONS ACT-GET DROP 100 ?S
        4807 42 50 ADD-ORDER
        4807 42 ACTION>KEY ACTIONS ACT-GET DROP 100 ?S
        4807 42 200 ADD-ORDER
        4807 42 ACTION>KEY ACTIONS ACT-GET DROP 200 ?S

        ." adding orders then computing profit maximizes profit" CR
        INIT-ACTIONS
        0 5 100 ADD-ORDER
        3 7 140 ADD-ORDER
        5 9 80  ADD-ORDER
        6 9 70  ADD-ORDER
        CALC-PROFIT
        PROFIT @  180 ?S
}T
BYE

