\ Spike.fs   --- solving the RENT problem in gforth !! --- 

10000 CONSTANT MAX-ORDERS
VARIABLE #ORDERS
CREATE ORDERS 3 CELLS MAX-ORDERS * ALLOT
CREATE PROFITS MAX-ORDERS 1+ ALLOT

: !++ ( n,addr -- addr+cell  store value n at addr then leave address + 1 cell )
    SWAP    ( addr,n )
    OVER    ( addr,n,addr )
    !       ( addr )
    CELL+ ; ( addr+cell )

: ORDER!++ ( t,d,p,addr -- addr+3 cells  store values of an order at addr, then leave addr+3 cells )
    SWAP ROT  (   
