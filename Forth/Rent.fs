\ Rent.fs

REQUIRE ffl/act.fs

VARIABLE PROFIT
ACT-CREATE ACTIONS
ACT-CREATE PLAN

: [@] ( k t -- n   t[k] or 0 )
    ACT-GET 0= IF 0 THEN ;

: [!] ( n k t -- store n at k in t is n is greater )
    2DUP 2>R [@] MAX  
    2R> ACT-INSERT ; 


: CASH ( t -- update profit with plan at time t if it's greater )
    PLAN [@] PROFIT @ MAX 
    PROFIT ! ;

: RENT ( t d p -- update plan at time t+d with profit + p if it's greater )
    ROT DUP CASH 
    SWAP PROFIT @ +
    -ROT + PLAN [!] ;
    
: TIME>KEY ( t d -- k  encode a key from time and duration )
    SWAP 32 LSHIFT OR ;

: KEY>TIME ( k -- t d  decode time and duration form a key )
    DUP 32 RSHIFT
    SWAP -1 32 RSHIFT AND ;
    
: {CASH} ( t -- record a cash action at time t )
    0 TUCK TIME>KEY ACTIONS [!] ;

: {RENT} ( p t d  -- record a rent action if p is greater )
    TIME>KEY ACTIONS [!] ;

: ADD-ORDER ( t d p -- record actions for an order )
    -ROT 2DUP 
    + {CASH}
    {RENT} ;

: DO-ACTION ( n k -- perform action )
    KEY>TIME ?DUP IF 
    ROT RENT ELSE 
    NIP CASH THEN ; 

' DO-ACTION CONSTANT EXEC

: COMPUTE-PROFIT ( -- )
    PLAN ACT-INIT
    0 PROFIT !
    EXEC ACTIONS ACT-EXECUTE ;

: INIT ( -- )
    ACTIONS ACT-INIT ;

4096 CONSTANT MAX-LINE

: EVAL-STDIN ( -- read stdin and interpret it or 0 )
    PAD MAX-LINE STDIN READ-LINE THROW
    IF PAD SWAP EVALUATE ELSE 0 THEN ;

: PROCESS-CASE ( n -- process a number of orders )
    INIT
    EVAL-STDIN 0 DO 
        EVAL-STDIN ADD-ORDER 
    LOOP
    COMPUTE-PROFIT 
    PROFIT ? CR ;

: PROCESS-CASES ( -- process all cases )
    EVAL-STDIN 0 DO 
        PROCESS-CASE 
    LOOP ;

PROCESS-CASES
BYE
