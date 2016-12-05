    \ Rent.fs
    \ Solving the RENT problem in gforth

    CREATE PLAN  200 CELLS ALLOT  PLAN 200 CELLS ERASE   

    : []  ( i t -- n  retrieve value of position i in table t or 0 )
        SWAP CELLS + @ ;

    : [!] ( n i t -- update position i in table t with n if not smaller )
        SWAP CELLS +        ( n adr )
        DUP @               ( n adr n' )
        ROT MAX             ( adr v )
        SWAP ! ;             
        
