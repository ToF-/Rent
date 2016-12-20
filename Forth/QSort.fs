\ QSort.fs

: CELL- CELL - ;

: LT-PIVOT ( p a -- p a' first address of value >= p starting from p+1 )
    BEGIN 2DUP @ > WHILE CELL+ REPEAT ; 

: GT-PIVOT ( p a -- p a' first address of value <= p starting from p-1 )
    BEGIN 2DUP @ < WHILE CELL- REPEAT ; 

: EXCHANGE ( a b -- swap address a and b content )
    2DUP @ SWAP @ ROT ! SWAP ! ;

: LT-PIVOT-GT ( p h l -- p h l  finds limit positions around pivot )
     ROT SWAP LT-PIVOT   \ h p l
    -ROT SWAP GT-PIVOT   \ l p h
     ROT ;               \ p h l

: LT++HT--  ( h l -- h-1 l+1  increase and decrease positions around pivot )
    CELL+ SWAP CELL- SWAP ;
 
: PARTITION ( h l -- h  partitions the array, leaving new pivot )
    DUP @ -ROT  \ p h l
    BEGIN
        LT-PIVOT-GT        \ p h l
        2DUP > IF          \ p h l
            2DUP EXCHANGE LT++HT--   
        ELSE DROP NIP EXIT THEN
    AGAIN ;

: QSORT ( h l -- sort the array )
    2DUP > IF 
        2DUP PARTITION DUP \ h l p p
        ROT   RECURSE     
        CELL+ RECURSE
    ELSE 2DROP THEN ;

    
: SORT ( a l -- sort the array a of size l )
    CELLS OVER + SWAP QSORT
;
