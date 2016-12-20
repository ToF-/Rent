 \ QSortTest.fs

REQUIRE ffl/tst.fs
REQUIRE QSort.fs

T{
    CREATE A
    4807 ,   42 ,  512 , 4096 , 1000 , 256 , 

    ." LT-PIVOT seek address of 1st item >= pivot" CR
        4807 A CELL- LT-PIVOT NIP   A ?S  
        512  A       LT-PIVOT NIP   A 2 CELLS + ?S

    ." GT-PIVOT seek address of 1st item <= pivot" CR
        4807 A 6 CELLS + GT-PIVOT NIP   A 5 CELLS + ?S  
        42   A 6 CELLS + GT-PIVOT NIP   A 1 CELLS + ?S  

}T
BYE
