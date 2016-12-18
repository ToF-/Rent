\ Rent.fs
\ Solving the RENT problem in gforth

10000 CONSTANT MAXORDERS 
VARIABLE #ORDERS
CREATE ORDERS MAXORDERS CELLS ALLOT

: INIT-ORDERS ( --  initialize the list of orders )
    ORDERS MAXORDERS ERASE
    0 #ORDERS ! ;

: ENCODE-ORDER ( t d p -- o   encode time duration and price into an order record )
    ROT 1000000 * ROT + 100000 * + ;

: DECODE-ORDER ( o -- t d p   decode time duration and price from an order record )
    100000 /MOD 1000000 /MOD SWAP ROT ;
    
: ADD-ORDER ( t d p -- encode an order an add it to the list )
    ENCODE-ORDER
    ORDERS #ORDERS @ CELLS + ! 
    1 #ORDERS +! ;  

: BSEARCH ( x h l -- n finds the position of first v >= x )
    ROT >R
    BEGIN 2DUP - 1 > WHILE
        2DUP + 2 /             
        DUP CELLS ORDERS +     
        @ R@ < IF SWAP DROP ELSE SWAP ROT DROP THEN
    REPEAT DROP R> DROP ;
        
        
: NEAREST ( t -- n  finds the position of the nearest order to t)
    0 0 ENCODE-ORDER #ORDERS @ 0 BSEARCH ;

