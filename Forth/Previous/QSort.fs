\ QSort.fs

: LT-PIVOT ( p,a -- p,a'  first address a'> a and [a'] >= p )
    BEGIN CELL + 2DUP @ > WHILE REPEAT ; 

: GT-PIVOT ( p,a -- p,a'  first address a'< a and [a'] <= p ) 
    BEGIN CELL - 2DUP @ < WHILE REPEAT ; 

: EXCHANGE ( a,b --       swap [a] and [b] )
    2DUP @ SWAP @ ROT ! SWAP ! ;

: LT-PIVOT-GT ( p,h,l -- p,h',l'  limits around pivot )
     ROT SWAP LT-PIVOT ROT   
     ROT SWAP GT-PIVOT ROT ;            

: PARTITION ( h,l -- h  partitions the array, leaving new pivot )
    DUP @ -ROT               \ pivot = [l]
    CELL - SWAP CELL + SWAP
    BEGIN 
        LT-PIVOT-GT 
        2DUP > IF 
        2DUP EXCHANGE ELSE 
        DROP NIP EXIT THEN
    AGAIN ;

: QSORT ( h,l -- sort the array )
    2DUP > IF 
        2DUP PARTITION DUP \ h l p p
        ROT    RECURSE     
        CELL + RECURSE
    ELSE 2DROP THEN ;

    
: SORT ( a l -- sort the array a of size l )
    CELLS OVER + SWAP QSORT
;
