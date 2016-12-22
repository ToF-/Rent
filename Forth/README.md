<link rel="stylesheet" href="./story.css">
#RENT in FORTH!

Let's recap the algorithm at the center of the program that we want to implement:
> given:<br>
>    O, an array of length N containing orders, each order being defined by its start time, duration, and price;<br>
>    B, an array of length N+1 containing the best profit at the start time of each order, except for B[N] = 0;<br>
> 
> sort O by order start time;<br>
> for i starting at N-1 until 0:<br>
>   compute B[i] : max(price(O[i]) + B[k], B[i+1])<br>
>   where k = mimimum[ j | start_time(O[j]) ≥ start_time(O[i]) + duration(O[i])
>
> B[0] = best profit for the array of orders.

We have many small and less small problems to solve. Let's launch `gforth`:

    > gforth ⏎
    Gforth 0.7.2, Copyright (C) 1995-2008 Free Software Foundation, Inc.
    Gforth comes with ABSOLUTELY NO WARRANTY; for details type `license'
    Type `bye' to exit

#RENT Algorithm

Here's the algorithm that will solve the problem:

>*Given*:
>
>- *O, an array of N orders sorted by start time,*
>
>- *B, an array of N+1 integers representing the best profit at the start time of each order*
>
>*start with B[N] = 0, for i starting at N-1 until 0: compute B[i] = max(O[i].price + B[k], B[i+1])*
>
>*where k = minimum { j | j > i, O[j].start >= O[i].start + O[i].duration }*
>
>*then B[0] = best profit for the list of orders.*

#FORTH implementation

Let's decompose our problem into smaller problems:

1. Using integer cell values to represent orders
2. Sorting an arrays of cell values
3. Searching efficiently an array of cells for a value >= x
4. Reading and processing each case in the standard input

##Writing Tests

Before coding anything we should make sure that we can write tests. We will rely on the [`tst`](http://irdvo.nl/FFL/docs/tst.html) library. Here's an example:

    \ Tests.fs
    REQUIRE ffl/tst.fs

    T{
        ." dummy test" CR
            2 3 +  4 ?S
    }T
    BYE

Here's what happens when we launch a test that fails:

>     gforth Tests.fs ⏎
>     dummy test
>     stack contents mismatch:     2 3 +  4 ?S
>     expecting 4 and found 5

Making the test pass:

    \ Tests.fs
    REQUIRE ffl/tst.fs

    T{
        ." dummy test" CR
            2 3 +  4 ?S
    }T
    BYE

Here's what happens when we launch a test that passes:

>     gforth Tests.fs ⏎
>     dummy test

## Storing order information

How to represent an order in Forth ? The language can easily be extended to support *struct* but we have to ask ourselves if it is the slimplest thing that could work. What do we need to do with orders ?

- given an order start time t and duration d, calculate t+d, the order end time
- sort an array of orders by start time
- given an end time t, and a sorted array of orders, find the first order having start time >= t

Maybe we can use *cells* to store orders. That would be practical, because so many Forth words use cells. In *gforth* a cell occupies 8 bytes in memory. Since the largest values for an order start time, duration and price are 999999, 999999 and 99999 respectively, we can safely store these 3 fields in a single cell. For example the order defined by:
    
- start time = 480712 
- duration   = 932768
- price      = 4232

Could be encoded in a cell with value = 48 071 293 276 804 232. Such a value would still stand on a single 8 byte signed cell, since the max positive integer value for such a cell is 2^63, 4 611 686 018 427 387 904.

It would be convenient to encode the order fields in a way that if we have
    
- o  = (t  < 1000000,d  < 1000000, p  < 100000)
- o' = (t' < 1000000,d' < 1000000, p' < 100000)

then the following is always true:

- t < t' <=> encode(o) < encode(o')
- t > t' <=> encode(o) > encode(o')

The way to do this is to define the start time as the *most significant* part of the cell value.  

Of course, this particular choice of representation of an order should be transparent for the rest of the program.
 
Let's write a test:

    REQUIRE ffl/tst.fs
    REQUIRE Rent.fs

    T{

    ." an order can be encoded and decoded from a cell" CR
        0 5 100 ENCODE-ORDER
        3 7 140 ENCODE-ORDER
        SWAP
        DECODE-ORDER 100 ?S 5 ?S 0 ?S
        DECODE-ORDER 140 ?S 7 ?S 3 ?S

    }T
    BYE

Creating an order record is done by multiplying the start time by 10⁶, then adding the duration, then multiplying this number by 10⁵, then adding the price.

    \ Rent.fs
    \ Solving the RENT problem in gforth

    : ENCODE-ORDER ( t,d,p -- n   encode time, duration, price on one cell )
        ROT  1000000 * 
        ROT + 100000 * + ;

Getting the values from an order record is done by dividing the value by 10⁵, letting the modulo on the stack as the price, then dividing the quotient by 10⁶, obtaining the duration and time, then rearranging the values on the stack.

    : DECODE-ORDER ( n -- t,d,p   decode time, duration, price from a cell )
         100000 /MOD 
        1000000 /MOD SWAP ROT ;

Let's verify that this encoding allows us to sort the orders by time value:

    ." order as cells can be compared on time value" CR
        3      7  140 ENCODE-ORDER
        2 999999 9999 ENCODE-ORDER
        > ?TRUE 

Finally, to tell if an order compatible with a given time, we just need to 

- a) encode that time as an "empty" order 
- b) compare the cell values: 

Let's write a simple test:

    ." order as cell can be said to start after a time" CR
        5 0  0 ENCODE-ORDER
        5 9 80 ENCODE-ORDER
        <= ?TRUE

