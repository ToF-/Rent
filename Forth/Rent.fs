    \ Rent.fs
    \ Solving the RENT problem in gforth

    CREATE PLAN  200 CELLS ALLOT  PLAN 200 CELLS ERASE   

    : CELLS+ ( i t -- adr  compute adress of position i in table t )
        SWAP CELLS + ;

    : []  ( i t -- n  retrieve value of position i in table t or 0 )
       CELLS+ @ ;

    : [!] ( n i t -- update position i in table t with n if not smaller )
        CELLS+ DUP @
        ROT MAX
        SWAP ! ;
        
