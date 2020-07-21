: MAKE-ORDER ( s,d,p -- p,sd )
    ROT 32 LSHIFT ROT OR ;

: START-TIME ( p,sd -- s )
    NIP 32 RSHIFT ;

HEX
FFFFFFFF CONSTANT DURATION-MASK
DECIMAL
: DURATION ( p,sd -- d )
    NIP DURATION-MASK AND ;

: PRICE ( p,sd -- p )
    DROP ;

: ORDER! ( p,sd,a -- )
    2! ;

: ORDER@ ( a -- p,sd )
    2@ ;

: ORDER-POSITION ( p,sd -- sd )
    NIP ;

: ADD-DURATION ( p,sd -- p,s+d0 )
    NIP
    DUP DURATION-MASK AND 
    SWAP 32 RSHIFT +
    32 LSHIFT 
    0 SWAP ;

: COMPATIBLE? ( p,sd,p',sd' -- f )
    ADD-DURATION 
    D>= ;

: ORDER@-ARRAY ( a,p,l -- )
    0 DO
        OVER OVER !
        CELL+ SWAP CELL+ CELL+ SWAP
    LOOP 2DROP ;

: POSITION@ ( a -- sd )
    @ ORDER@ ORDER-POSITION ;

: PRICE@ ( a -- p )
    @ ORDER@ PRICE ;

: MID ( l,h -- m )
    OVER - 2/ ALIGNED + ;

: EXCH ( l,h -- )
    DUP @ >R
    OVER @ SWAP !
    R> SWAP ! ;

: CELL- 
    8 - ;

: PARTITION ( l,h -- l',h',h'',l'' )
    2DUP MID POSITION@ >R
    2DUP BEGIN
        SWAP BEGIN                       \ l,h,h,l 
            DUP POSITION@ R@ < WHILE CELL+ 
        REPEAT \ l,h,h,l'  | [l..l'[ < P
        SWAP BEGIN                       \ l,h,l',h
            R@ OVER POSITION@ < WHILE CELL- 
        REPEAT
        2DUP <= IF 2DUP EXCH >R CELL+ R> CELL- THEN
        2DUP > UNTIL
    R> DROP ;

: QSORT ( l,h -- )
    PARTITION SWAP ROT
    2DUP < IF RECURSE ELSE 2DROP THEN
    2DUP < IF RECURSE ELSE 2DROP THEN ;

: SORT ( a,l -- )
    DUP 2 >= IF 1- CELLS OVER + QSORT
             ELSE 2DROP THEN ;

: NEXT-COMPATIBLE ( p,sd,pa -- pa' )
    -ROT ADD-DURATION ORDER-POSITION >R
    BEGIN DUP POSITION@ R@ < WHILE CELL+ REPEAT
    R> DROP ;

: PRINT-ORDER ( p,sd -- )
    2DUP START-TIME 10 .R
    2DUP DURATION   10 .R
    PRICE           10 .R ;

: ADD-SENTINEL ( a,l -- )
    2* CELLS + >R
    2000000 0 0 MAKE-ORDER R> ORDER! ;

: ORDERS-VALUE ( a,l -- n )
    1- CELLS OVER + CELL-   \ a0,ai
    2DUP BEGIN              \ a0,ai,a0,ai
        >R
        R@ @ ORDER@ R@ CELL+ NEXT-COMPATIBLE
        PRICE@ R@ PRICE@ +
        R@ CELL+ PRICE@ MAX
        R@ @ ORDER@ NIP R@ @ ORDER! 
        R> CELL-
    2DUP > UNTIL 
    2DROP DROP
    PRICE@ ;

: PRINT-ORDER-ARRAY ( a,l )
    0 DO DUP I CELLS + @ ORDER@ PRINT-ORDER CR LOOP DROP ;

CREATE INPUT-LINE 40 ALLOT

: READ-NUMBERS
    INPUT-LINE DUP 40 STDIN READ-LINE THROW 
    IF EVALUATE ELSE 2DROP THEN ;    

VARIABLE MAX-ORDER
CREATE ORDERS 10001 2* CELLS ALLOT
CREATE POINTERS 10001 CELLS ALLOT

: GET-ORDERS ( -- )
    READ-NUMBERS DUP MAX-ORDER !
    0 DO
        READ-NUMBERS MAKE-ORDER
        ORDERS I 2* CELLS + ORDER!
    LOOP ;

: PROCESS-ORDERS ( -- n )
    ORDERS MAX-ORDER @ ADD-SENTINEL
    1 MAX-ORDER +!
    ORDERS POINTERS MAX-ORDER @ ORDER@-ARRAY
    POINTERS MAX-ORDER @ SORT
    POINTERS MAX-ORDER @ ORDERS-VALUE ;

: PROCESS 
    READ-NUMBERS 0 DO
        GET-ORDERS
        PROCESS-ORDERS
        . CR
    LOOP ;

PROCESS
BYE
