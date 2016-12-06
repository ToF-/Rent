    \ Rent.fs
    \ Solving the RENT problem in gforth

    CREATE PLAN  200 CELLS ALLOT  PLAN 200 CELLS ERASE   

    : PLAN[] ( t -- addr   address of position t in plan )
        CELLS PLAN + ;

    : PLAN@ ( t -- retrieve a value at position t from plan or 0 )
        PLAN[] @ ;

    : PLAN! ( n t -- stores value n at position t in plan )
        PLAN[] DUP @ 
        ROT MAX
        SWAP ! ;

    VARIABLE PROFIT

    : INITIALIZE ( -- put profit and plan to zero )
        0 PROFIT !
        PLAN 200 CELLS ERASE ;

    : CASH ( t -- update profit from plan at a given time )
        PLAN@ 
        PROFIT @ MAX
        PROFIT ! ;

    : RENT ( st du pr -- update plan according to rent )
        ROT DUP CASH   
        SWAP PROFIT @ + 
        -ROT + PLAN! ;
