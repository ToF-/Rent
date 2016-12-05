    \ Rent.fs
    \ Solving the RENT problem in gforth

    CREATE PLAN  200 CELLS ALLOT  PLAN 200 CELLS ERASE   

    : POS ( i t -- address of position i in table t )
        SWAP CELLS + ;

    : [@]  ( i t -- n  retrieve value at pos i in table t or 0 )
       POS @ ;

    : [!] ( n i t -- update pos i in t with n if greater )
        POS DUP @
        ROT MAX
        SWAP ! ;             

