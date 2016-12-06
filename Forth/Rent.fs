    \ Rent.fs
    \ Solving the RENT problem in gforth

    CREATE PLAN  200 CELLS ALLOT  PLAN 200 CELLS ERASE   

    : [] ( i t -- address of position i in table t )
        SWAP CELLS + ;

    : [@|0]  ( i t -- n  retrieve value at pos i in table t or 0 )
       [] @ ;

    : [>!] ( n i t -- update pos i in t with n if greater )
        [] DUP @
        ROT MAX
        SWAP ! ;             

    VARIABLE PROFIT

    : CASH ( t -- update profit from plan at a given time )
        PLAN [@|0] 
        PROFIT @ MAX
        PROFIT ! ;

    : RENT ( st du pr -- update plan according to rent )
        ROT DUP CASH   
        SWAP PROFIT @ + 
        -ROT + PLAN [>!] ;
        
