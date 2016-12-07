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
