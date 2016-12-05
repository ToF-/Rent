    REQUIRE ffl/tst.fs
    REQUIRE Rent.fs

    T{
    ." PLAN value (eg at 42) is O by default" CR
    42 PLAN []  ?0

    ." after updating the value can be retrieved" CR
    4807 42 PLAN [!]
    42 PLAN []  4807 ?S

    ." updating with a smaller value is not possible" CR
    4807 42 PLAN [!]
    4096 42 PLAN [!]
    42 PLAN []  4807 ?S
    }T
    BYE
