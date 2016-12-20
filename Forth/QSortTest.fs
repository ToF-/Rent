 \ QSortTest.fs

REQUIRE ffl/tst.fs
REQUIRE QSort.fs


: .. ( addr n -- prints n values from addr,addr+1,addr+2.. )
    0 DO DUP @ . CELL+ LOOP DROP ;

: [] ( addr n -- addr   address of item n in array addr )
    CELLS + ;
T{
    CREATE MYARRAY
    4807 ,   42 ,  512 , 4096 , 1000 , 256 , 

    ." LT-PIVOT seek address of 1st item >= pivot" CR
        4096 MYARRAY 0 [] CELL-  LT-PIVOT MYARRAY 0 [] ?S 4096 ?S
        4096 MYARRAY 1 [] CELL-  LT-PIVOT MYARRAY 3 [] ?S 4096 ?S
    
    ." GT-PIVOT seek address of 1st item <= pivot" CR
        256  MYARRAY 5 [] CELL+  GT-PIVOT MYARRAY 5 [] ?S  256 ?S
        4096 MYARRAY 4 [] CELL+  GT-PIVOT MYARRAY 4 [] ?S 4096 ?S
        42   MYARRAY 5 [] CELL+  GT-PIVOT MYARRAY 1 [] ?S   42 ?S

    ." EXCHANGE exchanges two items" CR
        MYARRAY 5 [] MYARRAY 0 [] EXCHANGE
        MYARRAY 5 [] @ 4807 ?S
        MYARRAY 0 [] @ 256  ?S
        MYARRAY 5 [] MYARRAY 0 [] EXCHANGE

    ." PARTITION separates items lower and greater than a pivot, returning address of next pivot" CR
        MYARRAY 5 [] MYARRAY 0 [] PARTITION MYARRAY 4 [] ?S
        MYARRAY 6 .. CR
        MYARRAY 4 [] MYARRAY 0 [] PARTITION MYARRAY 0 [] ?S
        MYARRAY 6 .. CR
        MYARRAY 5 [] MYARRAY 4 [] PARTITION MYARRAY 0 [] ?S
        MYARRAY 6 .. CR
}T
BYE

