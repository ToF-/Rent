\ Rent.fs

REQUIRE ffl/act.fs

VARIABLE PROFIT
ACT-CREATE ACTIONS
ACT-CREATE PLAN

: [@] ( k t -- n   t[k] or 0 )
    ACT-GET 0= IF 0 THEN ;

: [!] ( n k t -- store n at k in t is n is greater )
    2DUP >R >R 
    [@] MAX  
    R> R> ACT-INSERT ; 


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
    0 TIME>KEY 0 SWAP ACTIONS [!] ;

: {RENT} ( t d p -- record a rent action if p is greater )
    -ROT TIME>KEY ACTIONS [!] ;

: ADD-ORDER ( t d p -- record actions for an order )
    -ROT 2DUP + {CASH}
    ROT {RENT} ;

: DO-ACTION ( n k -- perform action )
    KEY>TIME  
    DUP 0= IF 
        DROP CASH DROP 
    ELSE ROT RENT THEN ;

' DO-ACTION CONSTANT EXEC

: COMPUTE-PROFIT ( -- )
    PLAN ACT-INIT
    0 PROFIT !
    EXEC ACTIONS ACT-EXECUTE ;

: INIT ( -- )
    ACTIONS ACT-INIT ;
