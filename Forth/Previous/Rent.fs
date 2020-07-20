\ Rent.fs
\ Solving the RENT problem in gforth

10000 CONSTANT MAX-ORDERS
VARIABLE #ORDERS
CREATE ORDERS  MAX-ORDERS 1+ CELLS ALLOT
VARIABLE @NEXT-ORDER

: ENCODE-ORDER ( t,d,p -- n  encode an order in a single cell )
    ROT  1000000 *  ( d,p,t__ )
    ROT + 100000 *  ( p,td_ )
    + ;             ( tdp )

: DECODE-ORDER ( n -- t,d,p  decode a cell into an order )
    100000  /MOD    ( p,td )
    1000000 /MOD    ( p,d,t )
    SWAP ROT ;      ( t,d,p )

: INITIALIZE ( -- sets variables to initial values )
    #ORDERS OFF
    ORDERS @NEXT-ORDER ! ;

: ADD-ORDER ( t,d,p -- add an order to the array )
    ENCODE-ORDER
    #ORDERS @ MAX-ORDERS < IF
        @NEXT-ORDER @ !
        CELL @NEXT-ORDER +!
        1 #ORDERS +! 
    ELSE
        DROP
    THEN ;

: NEAREST-ORDER ( t,addr -- find the nearest compatible order )
    SWAP 0 0 ENCODE-ORDER SWAP
    BEGIN
        2DUP @ >
    WHILE 
        CELL+
    REPEAT NIP ;
    
