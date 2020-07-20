32 CONSTANT DURATION%
HEX FFFFFFFF DECIMAL CONSTANT DURATION-MASK

CELL 2* CONSTANT ORDER%

: ORDERS% ( n -- m memory size on n orders )
    2* CELLS ;

: SDP->ORDER ( s,d,p -- p,sd )
    ROT DURATION% LSHIFT 
    ROT OR ;

: START-TIME ( p,sd -- s )
    SWAP DROP DURATION% RSHIFT ;

: DURATION ( p,sd -- d )
    NIP DURATION-MASK AND ;

: PRICE ( p,sd -- p )
    DROP ;

: ADD-PRICE ( p,sd,q -- p+q,sd )
    M+ ;

: ADD-DURATION ( p,sd -- p,s+d0 )
    DUP DURATION-MASK AND  \ p,sd,d
    SWAP DURATION% RSHIFT  \ p,d,s
    + DURATION% LSHIFT  ;  \ p,s+d0

: COMPATIBLE ( p,sd0,p1,sd1 -- sd0 >= s1+d1 )
    ADD-DURATION D>= ;

: ORDER! ( p,sd,a -- )
    ROT OVER ! CELL + ! ;

: ORDER@ ( a -- p,sd )
    DUP @ SWAP CELL + @ ;

: >SPACES ( a,l -- a',l' advance to first non space )
    BEGIN
        OVER C@ 32 = WHILE 
        1- SWAP 1+ SWAP 
    REPEAT ;

: >USNUMBERS ( a,l -- u1,u2,... read single numbers from a string )
    BEGIN
        >SPACES
        DUP WHILE
        0 S>D 2SWAP >NUMBER ROT DROP
    REPEAT 2DROP ;

CREATE ORDERS 10000 2* CELLS ALLOT

VARIABLE MAX-ORDER

: PRINT-ORDERS
    MAX-ORDER @ 0 DO
        ORDERS I ORDERS% + ORDER@
        2DUP START-TIME .
        2DUP DURATION .
        PRICE .
        CR 
    LOOP ;

: LT-PIVOT ( dp,a -- dp,a' | a' > a && [a'] >= p )
    BEGIN 
        ORDER% + DUP   \ dp,a,a 
        2OVER ROT 2@   \ dp,a,dp,[da]
        D> WHILE       \ dp,a
    REPEAT ;

: GT-PIVOT ( p,a -- p,a' | a' < a && [a'] <= p )
    BEGIN 
        ORDER% - DUP  \ dp,a,a
        2OVER ROT 2@  \ dp,a,dp,[da]
        D< WHILE      \ dp,a
    REPEAT ;

: EXCHANGE ( a,b -- swap [da] and [db] )
    OVER    \ a,b,a
    2@      \ a,b,[da]
    ROT DUP \ a,[da],b,b
    2@      \ a,[da],b,[db]
    ROT >R  \ a,[da],[db]
    2SWAP   \ a,[db],[da]
    R> 2!   \ a,[db]     --  b<-[da]
    ROT 2! ; \ -- a <- [db]

: LT-PIVOT-GT ( dp,h,l -- dp,h',l'   limits around pivot )
    2OVER ROT  \ dp,h,dp,l
    LT-PIVOT   \ dp,h,dp,l'
    
    ROT SWAP GT-PIVOT ROT ;

: PARTITION ( h,l -- p  partitions the array, leaving new pivot )
    DUP @ -ROT                \ p,h,l   p=[l]
    CELL - SWAP CELL + SWAP   \ p,h+1,l-1
    BEGIN
        LT-PIVOT-GT           \ p,h',l'
        2DUP > IF
            2DUP EXCHANGE
        ELSE
            DROP NIP EXIT
        THEN
    AGAIN ;

: QSORT ( h,l -- sort the array )
    2DUP > IF
        2DUP PARTITION 
        DUP  \  h l p p
        ROT RECURSE         \  h p
        CELL + RECURSE      
    ELSE 2DROP THEN ;

: SORT ( a,l -- sort array a of size l )
    CELLS OVER + SWAP QSORT ;

CREATE LINE 40 ALLOT 

: READ-INPUT-LINE
    LINE 40 STDIN READ-LINE THROW ;

: GET-NUMBERS ( -- d1,d2.. )
    READ-INPUT-LINE 
    IF LINE SWAP EVALUATE THEN ;


: GET-ORDERS ( -- )
    GET-NUMBERS 
    MAX-ORDER !
    MAX-ORDER @ 0 DO
        GET-NUMBERS 
        SDP->ORDER 
        ORDERS I ORDERS% + ORDER! 
    LOOP ;

VARIABLE MAX-CASE

: PROCESS
    GET-NUMBERS 
    MAX-CASE !
    MAX-CASE @ 0 DO 
        GET-ORDERS 
        PRINT-ORDERS
    LOOP 
    STDOUT CLOSE-FILE THROW ;

\ PROCESS BYE

