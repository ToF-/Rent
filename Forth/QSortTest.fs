\ QSortTest.fs

REQUIRE ffl/tst.fs

: PARTITION ( h l -- p  partition and return new partition pos )
    DUP >R
    CELL- SWAP CELL+ SWAP \ j i  -- i = l-1, j = h+1
    BEGIN
           BEGIN CELL+ DUP @ R@ < WHILE REPEAT
      SWAP BEGIN CELL- DUP @ R@ > WHILE REPEAT
      SWAP 2DUP < IF 2DUP EXCHANGE TRUE 
                ELSE 
    

T{
    CREATE MYLIST 
    4807 , 42 , 512 , 4096 , 1000 , 256 , 

    ." partition separates items smaller and greater than pivot"
    MYLIST DUP 5 CELLS + SWAP
    PARTITION MYLIST 4 CELLS + ?S
   

     
    
}T
BYE

