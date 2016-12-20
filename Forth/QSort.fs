\ QSort.fs

: .SK .S KEY DROP ;

: CELL- ( addr -- addr )
    CELL - ;

: LT-PIVOT ( p lo -- p lo  seek for 1st value >= p in addr,addr+1,addr+2.. )
    BEGIN CELL+ 2DUP @ > WHILE REPEAT ;
     
: GT-PIVOT ( p hi -- p hi   seek for 1st value <= p in addr,addr-1,addr-2.. )
    BEGIN CELL- 2DUP @ < WHILE REPEAT ;

: EXCHANGE ( a b -- exchange the values in a and b )
    2DUP @ >R @ SWAP ! R> SWAP ! ;

: PARTITION ( j i -- adp separates items lower and greater than lo+0, leaving new pivot )
    DUP @                     \ j i p
    ROT CELL+ ROT CELL-       \ p j=j+1 i=i-1 
    BEGIN
        ROT SWAP  LT-PIVOT    \ j p i
        -ROT SWAP GT-PIVOT    \ i p j 
        ROT 2DUP              \ p j i j i
        >= IF                 \ p j i
            2DUP EXCHANGE TRUE \ p j i t
         ELSE FALSE THEN       \ p j i f
    WHILE                      \ p j i
    REPEAT
    ROT 2DROP ;                 \ j
