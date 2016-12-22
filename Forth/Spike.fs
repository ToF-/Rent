\ Spike.fs   --- solving the RENT problem in gforth !! --- 

10000 CONSTANT MAX-ORDERS
VARIABLE #ORDERS
CREATE ORDERS  MAX-ORDERS CELLS ALLOT
CREATE PROFIT  MAX-ORDERS 1+ CELLS ALLOT

: ORDER>CELL ( t,d,p -- n  encode an order in a single cell )
    ROT  1000000 *  ( d,p,t__ )
    ROT + 100000 *  ( p,td_ )
    + ;             ( tdp )

