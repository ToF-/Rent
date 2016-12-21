\ Rent.fs
\ Solving the RENT problem in gforth

REQUIRE Sort.fs

10000 CONSTANT MAXORDERS 
VARIABLE #ORDERS
CREATE ORDERS MAXORDERS CELLS ALLOT
CREATE PROFIT MAXORDERS 1+ CELLS ALLOT

: INIT-ORDERS ( --  initialize the list of orders )
    ORDERS MAXORDERS CELLS ERASE
    0 #ORDERS ! ;

: ENCODE-ORDER ( t,d,p -- n   encode time, duration, price on one cell )
    ROT  1000000 * 
    ROT + 100000 * + ;

: DECODE-ORDER ( n -- t,d,p   decode time, duration, price from a cell )
     100000 /MOD 
    1000000 /MOD SWAP ROT ;
    
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

: CALC-PROFIT ( -- n compute the best profit value for the orders )
    ORDERS #ORDERS @ SORT 
    PROFIT MAXORDERS 1+ CELLS ERASE 
    #ORDERS @ DUP 0 DO 
        DUP 1- I - DUP
        CELLS ORDERS + @ DECODE-ORDER
        -ROT + NEAREST CELLS PROFIT + @ + OVER 1+ CELLS PROFIT + @ MAX
        SWAP CELLS PROFIT + !
    LOOP DROP 
    PROFIT @ ;
        
         
