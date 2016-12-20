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
        CREATE MYLIST 
        4807 , 42 , 512 , 1000 , 1 , 4096 ,
        MYLIST 6 SORT
        MYLIST 0 CELLS + @ 1 ?S
        MYLIST 1 CELLS + @ 42 ?S
        MYLIST 2 CELLS + @ 512 ?S
        MYLIST 3 CELLS + @ 1000 ?S
        MYLIST 4 CELLS + @ 4096 ?S
        MYLIST 5 CELLS + @ 4807 ?S

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

##Adding orders to the list

When we initialize the list, the numbers of orders is set to zero:

    ." after initialize, max orders is set to zero" CR
        INIT-ORDERS
        #ORDERS @ 0 ?S

Let's make some declarations:

    10000 CONSTANT MAXORDERS 
    VARIABLE #ORDERS
    CREATE ORDERS MAXORDERS CELLS ALLOT

    : INIT-ORDERS ( --  initialize the list of orders )
        ORDERS MAXORDERS ERASE
        0 #ORDERS ! ;

When we add an order, an encoded record of order is stored in the list:

    ." after adding an order, order is incoded in the list" CR
        INIT-ORDERS
        0 5 100 ADD-ORDER 
        3 7 140 ADD-ORDER 
        #ORDERS @ 2 ?S
        ORDERS        @ DECODE-ORDER 100 ?S 5 ?S 0 ?S
        ORDERS CELL + @ DECODE-ORDER 140 ?S 7 ?S 3 ?S

Let's try the words we have already:

    \ Spike.fs

    REQUIRE RENT.FS 
    REQUIRE SORT.FS 
    INIT-ORDERS  
    5 9 81 ADD-ORDER   
    3 7 142 ADD-ORDER  
    0 5 103 ADD-ORDER  
    6 9 74 ADD-ORDER   
    ORDERS 4 SORT   
    : .ORDER ( t d p -- print an order ) ROT . SWAP . . ; 
    : .ORDERS #ORDERS @ 0 DO ORDERS I CELLS + @ DECODE-ORDER .ORDER CR LOOP ; 
    CR .ORDERS 
    BYE

running our spike:

    gforth Spike.fs ⏎
    0 5 103
    3 7 142
    5 9 81
    6 9 74

##Searching for the nearest item to a value in a list

We now need to implement an efficient search for the nearest order to a certain time value. For example, given this list of orders:

    - 0 5 100
    - 3 7 140
    - 5 9 80
    - 6 9 70

and looking for the nearest order that is compatible with the first order (0 5 100) in the list, we want to search the list for the time value 5. As the orders are encoded in the list, we don't need to decode them in order to do such a search: looking for the order (5 0 0) will work, because 30000007000140 < 50000000000000 < 50000009000080, and thus the high limit on our serch will always be the item we are looking for when the interval searched is reduce to 1.
So that we are able to look for a time value that is greater than all the time in the list, we have to start the high limit on the search at MAXORDERS rather than MAXORDERS - 1.

The algorithm for the binary search is explained below:

    seaching for the position of the nearest value to T 
    start with L= 0, H = MAXORDERS
    while H - L > 1
        M = L + (H - L)/2
        if T > list(M)
            L = M   
        else
            H = M 
    H is the position

For example serching for the nearest value to 30 in the list 0 11 22 33 44 :  
    
    
    T = 30 L = 0  H = 5
    H - L > 1
    M = 0 + (5 - 0) / 2 = 2
    T > 22
        L = 2
    H - L > 1
    M = 2 + (5 - 2) / 2 = 3
    T <= 33
        H = 3
    H - L = 1
    postion = 3

Let's write some tests:
    
    
    ." nearest finds the position of the nearest order with a given time in the order list" CR
        INIT-ORDERS
        0 5 100 ADD-ORDER
        3 7 140 ADD-ORDER  
        5 9 80 ADD-ORDER  
        6 9 70 ADD-ORDER
        ORDERS #ORDERS @ SORT 
        5  NEAREST 2 ?S
        10 NEAREST 4 ?S
        14 NEAREST 4 ?S
        1  NEAREST 1 ?S

Our definition consist in encoding an order with the given time, and searching for it in the array of orders, using a binary search strategy: