\ Spike.fs   --- solving the RENT problem in gforth !! --- 

10000 CONSTANT MAX-ORDERS
VARIABLE #ORDERS
CREATE ORDERS  MAX-ORDERS CELLS ALLOT
CREATE PROFIT  MAX-ORDERS 1+ CELLS ALLOT

: ORDER>CELL ( t,d,p -- n  encode an order in a single cell )
    ROT  1000000 *  ( d,p,t__ )
    ROT + 100000 *  ( p,td_ )
    + ;             ( tdp )

: CELL>ORDER ( n -- t,d,p  decode a cell into an order )
    100000  /MOD    ( p,td )
    1000000 /MOD    ( p,d,t )
    SWAP ROT ;      ( t,d,p )

: NEAREST ( t,addr -- addr  nearest order with start time >= t )
    BEGIN
        2DUP
        @ > 
    WHILE CELL+ 
    REPEAT NIP ;


: NEW-RANGE ( h,l,m,f -- m,l| h,m  adjust range accoring to flag )
    IF -ROT THEN NIP ;

: RANGE? ( h,l -- f  true if h-l> cell )
    - CELL > ;

: MIDDLE ( h,l -- m  middle of the range )
    + 2/ -8 AND ;
 
: BNEAREST ( t,h,l -- addr  nearest order with start time >= t )
    ROT >R
    BEGIN
        2DUP RANGE?
    WHILE
        2DUP MIDDLE
        DUP @ R@  >=
        NEW-RANGE
    REPEAT 
    DROP R> DROP ;    


    0 5 100 ORDER>CELL ORDERS !
    3 7 140 ORDER>CELL ORDERS 1 CELLS + !
    5 9  80 ORDER>CELL ORDERS 2 CELLS + !
    6 9  70 ORDER>CELL ORDERS 3 CELLS + !
    2000000 0 0  ORDER>CELL ORDERS 4 CELLS + !
    5 #ORDERS !
    0 0 0 ORDER>CELL ORDERS #ORDERS @ CELLS + ORDERS bnearest ? cr
    5 0 0 order>cell orders #orders @ cells + ORDERS bnearest ? cr
    2 0 0 order>cell orders #orders @ cells + ORDERS bnearest ? cr
    6 0 0 order>cell orders #orders @ cells + ORDERS bnearest ? cr
    7 0 0 order>cell orders #orders @ cells + ORDERS bNEAREST ? CR
    bye

