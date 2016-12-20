 \ QSortTest.fs

REQUIRE ffl/tst.fs
REQUIRE QSort.fs

T{
    CREATE A
    4807 ,   42 ,  512 , 4096 , 1000 , 256 , 

    ." LT-PIVOT seek address of 1st item >= pivot" CR
        4807 A       LT-PIVOT NIP   A ?S  
        512  A CELL+ LT-PIVOT NIP   A 2 CELLS + ?S

    ." GT-PIVOT seek address of 1st item <= pivot" CR
        4807 A 5 CELLS + GT-PIVOT NIP   A 5 CELLS + ?S  
        42   A 5 CELLS + GT-PIVOT NIP   A 1 CELLS + ?S  

    ." EXCHANGE swaps the content of two addresses" CR
        A A 2 CELLS + EXCHANGE A @ 512 ?S A 2 CELLS + @ 4807 ?S  
        A A 2 CELLS + EXCHANGE

    ." PARTITION separate items < and >= to pivot, leaving next pivot" CR
        A 5 CELLS + A PARTITION   A 4 CELLS + ?S
        A 5 CELLS + @ 4807 ?S  A @ 256 ?S

    ." SORT quick sorts an array" CR
        A 5 SORT
        A DUP @ 42 ?S CELL+ DUP @ 256 ?S CELL+ DUP @ 512 ?S CELL+ DUP @ 1000 ?S CELL+ DUP @ 4096 ?S CELL+ @ 4807 ?S
}T
BYE
