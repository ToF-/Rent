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

: ADD-DURATION ( p,sd -- p,s+d0 )
    NIP
    DUP DURATION-MASK AND 
    SWAP 32 RSHIFT +
    32 LSHIFT 
    0 SWAP ;

: COMPATIBLE? ( p,sd,p',sd' -- f )
    ADD-DURATION 
    D>= ;

: ORDER-EXCHANGE ( a,a' -- exchange order contents )
    DUP ORDER@ >R >R  \ a,a' -- {sd',p'}
    OVER ORDER@       \ a,a',p,sd
    ROT ORDER!        \ a -- a' <- p,sd
    R> R>             \ a,p',sd'
    ROT ORDER! ;      \ a <- p',sd'

