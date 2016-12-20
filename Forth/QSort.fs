\ QSort.fs

: CELL- CELL - ;

: LT-PIVOT ( p a -- p a' first address of value >= p starting from p+1 )
    BEGIN 2DUP @ > WHILE CELL+ REPEAT ; 

: GT-PIVOT ( p a -- p a' first address of value <= p starting from p-1 )
    BEGIN 2DUP @ < WHILE CELL- REPEAT ; 

: EXCHANGE ( a b -- swap address a and b content )
    2DUP @ SWAP @ ROT ! SWAP ! ;

: PARTITION ( h l -- h  partitions the array, leaving new pivot )
    DUP @   \ h l p
    BEGIN
        SWAP LT-PIVOT SWAP \ h l p  
        ROT GT-PIVOT ROT   \ p h l
        2DUP > IF          \ p h l
            2DUP EXCHANGE  \ p h l
            CELL+ SWAP CELL- SWAP 
            ROT TRUE
        ELSE ROT 2DROP FALSE THEN
    WHILE 
    REPEAT ;

: QSORT ( h l -- sort the array )
    
: SORT ( a l -- sort the array a of size l )
    CELLS OVER + SWAP QSORT
;
