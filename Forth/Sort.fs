\ sort.fs source: http://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Forth

: -CELL -8 ;

: CELL- CELL - ;

: MID ( l r -- mid ) OVER - 2/ -CELL AND + ;

: EXCH ( addr1 addr2 -- ) DUP @ >R OVER @ SWAP ! R> SWAP ! ;

: PARTITION ( l r -- l r r2 l2 )
  2DUP MID @ >R ( r: pivot )
  2DUP BEGIN
    SWAP BEGIN DUP @  R@ < WHILE CELL+ REPEAT
    SWAP BEGIN R@ OVER @ < WHILE CELL- REPEAT
    2DUP <= IF 2DUP EXCH >R CELL+ R> CELL- THEN
  2DUP > UNTIL  R> DROP ;

: QSORT ( l r -- )
  PARTITION  SWAP ROT
  2DUP < IF RECURSE ELSE 2DROP THEN
  2DUP < IF RECURSE ELSE 2DROP THEN ;

: SORT ( array len -- )
  DUP 2 < IF 2DROP EXIT THEN
  1- CELLS OVER + QSORT ;
