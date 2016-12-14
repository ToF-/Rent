\ Rent.fs
\ Solving the RENT problem in gforth

REQUIRE ffl/act.fs

ACT-CREATE PLAN 

: ACT-@ ( k t -- n  retrieve a k in tree t or 0 )
    ACT-GET 0= IF 0 THEN ;

: ACT-! ( n k t -- store value n at position k in tree t if n is greater )
    2DUP ACT-@ ?DUP IF 
        >R ROT R> MAX -ROT
 THEN ACT-INSERT ;

VARIABLE PROFIT

: INITIALIZE ( -- put profit and plan to zero )
    0 PROFIT !
    PLAN ACT-INIT ;

: CASH ( t -- update profit from plan at a given time )
    PLAN ACT-@ 
    PROFIT @ MAX
    PROFIT ! ;

: RENT ( t d p -- update plan according to rent )
    PROFIT @ + 
    -ROT + PLAN ACT-! ;

: ACTION>KEY ( t d -- k  encode time and duration into an action key )
    SWAP 32 LSHIFT OR ;

-1 32 RSHIFT CONSTANT MASK

: KEY>ACTION ( k -- t d  decode a key into an action time and duration )
    DUP 32 RSHIFT
    SWAP MASK AND ; 

: {CASH} ( t -- d k  prepare data and key for a cash action to be stored )
    0 ACTION>KEY NIL SWAP ; 

: {RENT} ( t d p -- d k  prepare data and key for a rent action to be stored )
    -ROT ACTION>KEY  ; 

ACT-CREATE ACTIONS

: INIT-ACTIONS ( -- empty action tree )
    ACTIONS ACT-INIT ;

: ADD-ORDER ( t d p -- stores rent and cash actions for order )
    -ROT 2DUP + {CASH} ACTIONS ACT-! 
    ROT         {RENT} ACTIONS ACT-! ;

: PERFORM-ACTION ( d k -- perform the cash or rent action )
    KEY>ACTION ?DUP IF ROT RENT ELSE CASH DROP THEN ;

' PERFORM-ACTION CONSTANT EXEC

: CALC-PROFIT ( -- compute profit for orders added )
    INITIALIZE
    EXEC ACTIONS ACT-EXECUTE ;

4096 CONSTANT MAX-LINE

: GET-STDIN ( -- n f  read from stdin on pad, leaving lenght and flag )
    PAD MAX-LINE STDIN READ-LINE THROW ;

: EVAL-LINE ( -- read a line from stdin and evaluate it or leave 0 )
    GET-STDIN IF PAD SWAP EVALUATE ELSE 0 THEN ;

: GET-ORDERS ( -- read orders from stdin and add them )
    INIT-ACTIONS
    EVAL-LINE 0 DO 
        EVAL-LINE ADD-ORDER
    LOOP ;
    
: MAIN ( -- read cases from stdin, compute and print profit )
    EVAL-LINE 0 DO
        GET-ORDERS
        INITIALIZE
        CALC-PROFIT
        PROFIT ? CR
    LOOP ;
