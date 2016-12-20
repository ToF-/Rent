\ QSort.fs

: CELL- CELL - ;

: LT-PIVOT ( p a -- p a' first address of value >= p starting from p+1 )
    BEGIN CELL+ 2DUP @ > WHILE REPEAT ; 

: GT-PIVOT ( p a -- p a' first address of value <= p starting from p-1 )
    BEGIN CELL- 2DUP @ < WHILE REPEAT ; 
