\ Spike.fs   --- solving the RENT problem in gforth !! --- 

10000 CONSTANT MAX-ORDERS
VARIABLE #ORDERS
CREATE ORDERS 3 CELLS MAX-ORDERS * ALLOT
CREATE PROFITS MAX-ORDERS 1+ ALLOT
