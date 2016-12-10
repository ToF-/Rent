\ Rent.fs
\ Solving the RENT problem in gforth

REQUIRE ffl/act.fs

ACT-CREATE PLAN 

: PLAN@ ( t -- retrieve a value at position t from plan or 0 )
    PLAN ACT-GET 0= IF 0 THEN ;

: PLAN! ( n t -- stores value n at position t in plan )
    DUP PLAN@
    ROT MAX 
    SWAP PLAN ACT-INSERT ;

VARIABLE PROFIT

: INITIALIZE ( -- put profit and plan to zero )
    0 PROFIT !
    PLAN ACT-INIT ;

: CASH ( t -- update profit from plan at a given time )
    PLAN@ 
    PROFIT @ MAX
    PROFIT ! ;

: RENT ( t d p -- update plan according to rent )
    ROT DUP CASH   
    SWAP PROFIT @ + 
    -ROT + PLAN! ;

: ACTION>KEY ( t d -- k  encode time and duration into an action key )
    SWAP 1000000 * + ;

: KEY>ACTION ( k -- t d  decode a key into an action time and duration )
    1000000 /MOD SWAP ;

: {CASH} ( t -- d k  prepare data and key for a cash action to be stored )
    0 ACTION>KEY NIL SWAP ; 

: {RENT} ( t d p -- d k  prepare data and key for a rent action to be stored )
    -ROT ACTION>KEY  ; 

ACT-CREATE ACTIONS
: INIT-ACTIONS ( -- empty action tree )
    ACTIONS ACT-INIT ;

: ACT-UPDATE ( d k tree -- update tree only if k not present or d is greater )
    2DUP ACT-GET 
    IF >R ROT R> MAX -ROT THEN
    ACT-INSERT ;

: ADD-ORDER ( t d p -- stores rent and cash actions for order )
    -ROT 2DUP
    + {CASH} ACTIONS ACT-INSERT 
    ROT {RENT} ACTIONS ACT-UPDATE ;

: PERFORM-ACTION ( d k -- perform the cash or rent action )
    KEY>ACTION ?DUP IF ROT RENT ELSE CASH DROP THEN ;

' PERFORM-ACTION CONSTANT EXEC

: CALC-PROFIT ( -- compute profit for orders added )
    INITIALIZE
    EXEC ACTIONS ACT-EXECUTE ;
