\ Rent.fs
\ Solving the RENT problem in gforth

10000 CONSTANT MAX-ORDERS
VARIABLE #ORDERS
CREATE ORDERS  MAX-ORDERS 1+ CELLS ALLOT
VARIABLE LAST-ORDER

: ENCODE-ORDER ( t,d,p -- n  encode an order in a single cell )
    ROT  1000000 *  ( d,p,t__ )
    ROT + 100000 *  ( p,td_ )
    + ;             ( tdp )

: DECODE-ORDER ( n -- t,d,p  decode a cell into an order )
    100000  /MOD    ( p,td )
    1000000 /MOD    ( p,d,t )
    SWAP ROT ;      ( t,d,p )

: INITIALIZE ( -- sets variables to initial values )
    ORDERS LAST-ORDER ! ;
    
