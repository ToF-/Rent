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
        CREATE MYLIST 
        4807 , 42 , 512 , 1000 , 1 , 4096 ,
        MYLIST 6 SORT
        MYLIST 0 CELLS + @ 1 ?S
        MYLIST 1 CELLS + @ 42 ?S
        MYLIST 2 CELLS + @ 512 ?S
        MYLIST 3 CELLS + @ 1000 ?S
        MYLIST 4 CELLS + @ 4096 ?S
        MYLIST 5 CELLS + @ 4807 ?S

    ." after initialize, max orders is set to zero" CR
        INIT-ORDERS
        #ORDERS @ 0 ?S

    ." after adding an order, order is incoded in the list" CR
        INIT-ORDERS
        0 5 100 ADD-ORDER 
        3 7 140 ADD-ORDER 
        #ORDERS @ 2 ?S
        ORDERS        @ DECODE-ORDER 100 ?S 5 ?S 0 ?S
        ORDERS CELL + @ DECODE-ORDER 140 ?S 7 ?S 3 ?S

    ." nearest finds the position of the nearest order with a given time in the order list" CR
        INIT-ORDERS
        0 5 100 ADD-ORDER
        3 7 140 ADD-ORDER  
        5 9 80 ADD-ORDER  
        6 9 70 ADD-ORDER
        ORDERS #ORDERS @ SORT 
        5  NEAREST 2 ?S
        10 NEAREST 4 ?S
        14 NEAREST 4 ?S
        1  NEAREST 1 ?S

        

}T
BYE

