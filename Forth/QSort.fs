\ QSort.fs

: .SK .S KEY DROP ;

: CELL- ( addr -- addr )
    CELL - ;

: LT-PIVOT ( p lo -- lo  seek for 1st value >= p in addr,addr+1,addr+2.. )
    BEGIN CELL+ 2DUP @ > WHILE REPEAT NIP ;
     
: GT-PIVOT ( p hi -- hi   seek for 1st value <= p in addr,addr-1,addr-2.. )
    BEGIN CELL- 2DUP @ < WHILE REPEAT NIP ;

: EXCHANGE ( a b -- exchange the values in a and b )
    2DUP @ >R @ SWAP ! R> SWAP ! ;

: PARTITION ( j i -- adp separates items lower and greater than lo+0, leaving new pivot )
    DUP @ >R              \ j i     [p] 
    SWAP CELL+ SWAP CELL-
    BEGIN
        R@ SWAP               \ j p i
        LT-PIVOT              \ j i
        R@ ROT                \ i p j
        GT-PIVOT              \ i j
        2DUP < IF 2DUP EXCHANGE 1 ELSE 0 THEN
    WHILE
    SWAP REPEAT               \ j i
    R> DROP  NIP ;            \ i j
