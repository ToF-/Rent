#FORTH implementation

Let's decompose our problem into smaller problems:

1. Storing order information on an 8 byte cell so that we keep the program simple
2. Sorting a memory area seen as a sequence of orders, by start time
3. Binary searching a memory area seen as a sequence of orders 
4. Reading the standard input and processing each case

##Writing Tests

We will rely on the [`tst`](http://irdvo.nl/FFL/docs/tst.html) library.

    \ Tests.fs

    REQUIRE ffl/tst.fs
    T{
    ." dummy test" CR
    2 3 +  4 ?S
    }T
    BYE

Here's what happens when we launch a test that fails:

    gforth Tests.fs ⏎
    dummy test
    stack contents mismatch:     2 3 +  4 ?S
      expecting 4 and found 5

Making the test pass:

    \ Tests.fs

    REQUIRE ffl/tst.fs
    T{
    ." dummy test" CR
    2 2 +  4 ?S
    }T
    BYE

Here's what happens when we launch a test that passes:

    gforth Tests.fs ⏎
    dummy test

## Storing order information

In *gforth* a cell occupies 8 bytes in memory. It is enough to store all information from an order, since the largest "order record value" would be :
    
- start time = 1000000 
- duration   = 1000000
- price      = 100000

The cell value for such a record is:

       100 000 000 000 000 000

which is comprised between 2^59 and 2^60. Such an order record would still stand on a single 8 byte signed cell, since the max value for such a cell is 2^63:

    4 611 686 018 427 387 904

Let's write a test:

    REQUIRE ffl/tst.fs
    REQUIRE Rent.fs

    T{
            ." an order can be encoded and decoded from a cell" CR
                3 7 140 ENCODE-ORDER
                DECODE-ORDER 140 ?S 7 ?S 3 ?S
    }T
    BYE

Creating an order record is done by multiplying the start time by 10⁶, then adding the duration, then multiplying this number by 10⁵, then adding the price.

    \ Rent.fs
    \ Solving the RENT problem in gforth

    : ENCODE-ORDER ( t d p -- o   encode time duration and price into an order record )
        ROT 1000000 * ROT + 100000 * + ;

Getting the values from an order record is done by dividing the value by 10⁵, letting the modulo on the stack as the price, then dividing the quotient by 10⁶, obtaining the duration and time, then rearranging the values on the stack.

    : DECODE-ORDER ( o -- t d p   decode time duration and price from an order record )
        100000 /MOD 1000000 /MOD SWAP ROT ;

This encoding allows us to sort the records by time value:

    ." order records can be compared on time value" cr
        3 7 140 ENCODE-ORDER
        2 999999 9999 ENCODE-ORDER
        > ?TRUE 

##Sorting an array of cells

Sorting efficiently can be complicated matter, especially in Forth.
Here is a test for a `SORT` word that is accessible on [rosettacode](http://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Forth)

    ." sorting records with qsort" CR
        CREATE LIST 
        4807 , 42 , 512 , 1000 , 1 , 4096 ,
        LIST 6 SORT
        LIST 0 CELLS + @ 1 ?S
        LIST 1 CELLS + @ 42 ?S
        LIST 2 CELLS + @ 512 ?S
        LIST 3 CELLS + @ 1000 ?S
        LIST 4 CELLS + @ 4096 ?S
        LIST 5 CELLS + @ 4807 ?S

And here is a copy from *rosettacode*:

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
