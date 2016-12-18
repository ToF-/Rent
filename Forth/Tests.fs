REQUIRE ffl/tst.fs
REQUIRE Rent.fs
REQUIRE Sort.fs

T{
    ." an order can be encoded and decoded from a cell" CR
        3 7 140 ENCODE-ORDER
        DECODE-ORDER 140 ?S 7 ?S 3 ?S

    ." order records can be compared on time value" CR
        3 7 140 ENCODE-ORDER
        2 999999 9999 ENCODE-ORDER
        > ?TRUE 

    ." sorting records with qsort" CR
        CREATE LIST 
        4807 , 42 , 512 , 1000 , 1 , 4096 ,
        LIST 6 SORT
        LIST 0 CELLS + @ 1 ?S
        LIST 1 CELLS + @ 42 ?S
        LIST 2 CELLS + @ 512 ?S
        LIST 3 CELLS + @ 1000 ?S
        LIST 4 CELLS + @ 4096 ?S
        LIST 5 CELLS + @ 4807 ?S
}T
BYE

