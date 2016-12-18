\ Rent.fs
\ Solving the RENT problem in gforth

: ENCODE-ORDER ( t d p -- o   encode time duration and price into an order record )
    ROT 1000000 * ROT + 100000 * + ;

: DECODE-ORDER ( o -- t d p   decode time duration and price from an order record )
    100000 /MOD 1000000 /MOD SWAP ROT ;
    
