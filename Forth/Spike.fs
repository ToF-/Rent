\ Rent.fs 

3 CELLS CONSTANT ORDER-SIZE
10000   CONSTANT MAX-ORDERS

CREATE ORDERS
ORDER-SIZE MAX-ORDERS * ALLOT

VARIABLE #ORDERS

: NEXT-ORDER ( -- addr  next available position in the array )
    ORDERS #ORDERS @ ORDER-SIZE * + ;

: !++ ( n,addr -- addr+1c  store n at addr then increment adr )
    DUP CELL+ -ROT ! ;

: ADD-ORDER ( t,d,p -- store order in array and increment counter )
    SWAP ROT  
    NEXT-ORDER !++ !++ !++ DROP
    1 #ORDERS +! ;
