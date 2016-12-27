#RENT in FORTH!

Let's recap the algorithm at the center of the program that we want to implement:
> given:<br>
>    *O*, an array of length N containing orders, each order being defined by its *start time*, *duration*, and *price*;<br>
>    0 ≤ *start time* < 1000000 ; 0 < *duration* < 1000000 ; 0 < price < 100000<br> 
>    *B*, an array of length N+1 containing the best profit at the *start time* of each order, except for *B[N]* = 0;<br>
> 
> sort O by *start time*;<br>
> for i starting at N-1 until 0:<br>
>   *B[i]* : max(*price(O[i])* + *B[k]*, *B[i+1]*)<br>
>   where k = mimimum[ j | *start time(O[j])* ≥ *start time(O[i])* + *duration(O[i])*
>
> B[0] = best profit for the array of orders.

If preserving data is not important, this algorithm can even be simplified so that only an array of N+1 orders is used:
> given:<br>
>    O, an array of length N+1 containing orders, each order being defined by its *start time*, *duration*, and *price*;<br>
>    for each order O[i], 0 ≤ *start time* < 1000000 ; 0 < *duration* < 1000000 ; 0 < price < 100000<br> 
>    except for O[N] the last order for which *start time* = 2000000, *duration* = 0, *price* = 0
> 
> sort O by *start time*;<br>
> for i starting at N-1 until 0:<br>
>   *price(O[i])* ←  max(*price(O[i])* + *price(O[k])*, *price(O[i+1])*)<br>
>   where k = mimimum[ j | *start time(O[j])* ≥ *start time(O[i])* + *duration(O[i])*
>
> *price(O[0])* = best profit for the array of orders.
We have many small and less small problems to solve. Let's launch `gforth`:

    > gforth ⏎
    Gforth 0.7.2, Copyright (C) 1995-2008 Free Software Foundation, Inc.
    Gforth comes with ABSOLUTELY NO WARRANTY; for details type `license'
    Type `bye' to exit


## Using the Stack memory

Let's suppose we have an order given on the Stack: it's easy, we just enter a *start time*, a *duration*, and a *price*.

    3 7 140 ⏎ ok

Then we can easily compute and print the end time of the order, i.e. the time at which another order could add some profit:

    ROT ROT + . ⏎ 10 ok

And now the only thing remaining on the Stack is the *price*:

    . ⏎ 140 ok

To understand what happened, let's add a `.S` (print the content of the Stack) after each word we type:  

    3 7 140 .S ⏎  <3> 3 7 140  ok
    ROT     .S ⏎  <3> 7 140 3  ok
    ROT     .S ⏎  <3> 140 3 7  ok
    +       .S ⏎  <2> 140 10  ok
    .    .S ⏎  10 <1> 140  ok
    .   .S ⏎  140 <0>  ok

###✍
> *Entering a number puts this number on the Stack*<br>
> *`+ ( n,n -- n )` pull two values from the Stack, adds them and leaves the result on the Stack*<br> 
> *`. ( n -- )` pull the value at top of the Stack and print it*<br>
> *`ROT ( a,b,c -- b,c,a)` moves the 3rd value to the top of the Stack*<br>
> *`.S` print the content of the Stack (without altering it)*<br>

## Using the Dictionary: Constants and Variables

We certainly cannot store all our orders on the Stack: that would represent 30000 values for a maximal case, and on a standard gforth configuration the Stack can store no more than 2040 values. We can use the Dictionary memory: this is the memory that is used for all the new definitions we will create: constants, variables and new words. <br>
Let's define a new constant:

    10000 CONSTANT MAX-ORDERS ⏎  ok
    MAX-ORDERS . ⏎  10000 ok

When executed, a constant leaves its value on the Stack.<br>
Let's also define a variable to store the actual number of orders:

    VARIABLE #ORDERS ⏎  ok
    #ORDERS . ⏎  4458332328  ok

When executed a variable leaves its *address* on the Stack. It's up to us programmers to know what to do with it. We can *fetch* its value: 

    #ORDERS @ . ⏎ 0 ok

Or we can *store* a new value into it:

    42 #ORDERS ! ⏎  ok
    #ORDERS @ .  ⏎  42 ok
    #ORDERS OFF ⏎  ok
    #ORDERS @ . ⏎ 0 ok

###✍
> *`CONSTANT <name> ( n -- )` create a new entry `name` in the Dictionary for the constant `n`*<br>
> *`VARIABLE <name>` create a new entry `name` in the Dictionary for a variable*<br>
> *`@ ( addr -- n )` fetch the address on the Stack and leave the value of this address content*<br> 
> *`! ( n,addr -- )` store the value at the address given on the Stack*<br>
> *`OFF ( addr -- )` set the content of the address to zero*<br>

## Using the Dictionary: Arrays

Obviously, we are not going to create 10000 variables to store the orders: we need a way to store several values starting at a given address. We do that by creating a new entry -- let's call it `ORDERS`, in the Dictionary:

    CREATE ORDERS ⏎ ok

Then we must *reserve* the right amount of memory bytes to store our 10000 orders. How much should that be? We are not sure yet. Let's suppose that an order will occupy no more than 8 bytes of memory. In fact 8 bytes is what it take to store an integer value -- called a *cell* -- in gforth, and we will have 10000 orders, thus:

    MAX-ORDERS CELLS ALLOT  ⏎ ok

The word `CELLS` multiplies the value on the Stack by the standard size of a cell in bytes.

Now `ORDERS` is a new word in the Dictionary, and its effect is to put the address of our memory zone, which is also the address of the first order in the array, on the Stack:

    ORDERS . ⏎  4451815928 ok

Let's create another entry: we need another array of 10001 integers, representing the best profit for each order (plus an initial zero value). Let's call this new array `PROFIT`:

    CREATE PROFIT MAX-ORDERS 1+ CELLS ALLOT ⏎ ok
    PROFIT . ⏎  4451895968 ok

Now if subtract the `PROFIT` address from the `ORDERS` address:

    PROFIT ORDERS - . ⏎ 80040

We find the difference to be slighty above 80000 bytes, which the the size that we alloted for the `ORDERS` entry.

###✍
> *`CREATE <name> ( -- )` create a new entry `name` in the Dictionary*<br>
> *`ALLOT ( n -- )` allot n bytes of memory for the entry most recently created*<br>
> *`CELLS ( n -- m )` multiply the value on the Stack by the number of byte in a cell*<br>
> *`1+ ( n -- m )` add 1 to the value on the top of the Stack*<br>
> *`- ( a,b -- n )` pull two values a and b from the Stack, and leave a-b on the Stack*<br> 
 

## Programming with __gforth__ scripts

It's time to keep a source code for the program we are creating. Let's put what we discovered so far in gforth script file:

    \ Spike.fs   --- solving the RENT problem in gforth !! --- 

    10000 CONSTANT MAX-ORDERS
    VARIABLE #ORDERS
    CREATE ORDERS  MAX-ORDERS CELLS ALLOT
    CREATE PROFIT  MAX-ORDERS 1+ CELLS ALLOT

And we can launch __gforth__ with this script:

>     >forth Spike.fs ⏎
>     Gforth 0.7.2, Copyright (C) 1995-2008 Free Software Foundation, Inc.
>     Gforth comes with ABSOLUTELY NO WARRANTY; for details type `license'
>     Type `bye' to exit
>     PROFITS ORDERS - . 80040  ok
>     #ORDERS ? 0  ok

###✍
> *`gforth <file>` launch gforth and execute everything that is included in the given file*<br>
> *`? ( addr -- )` print the value stored ut addr*<br>

## Storing orders

As we defined the entry that will keep the orders, we surmised that each order would fit in an 8-byte cell. Two questions arise: 1) are we sure? 2) how to encode reliably three integers in one cell?

### The size of an order

How many different values of order can we have? Let's recall the problem specification: 
> Each order is described by 3 integer values: The start time of the order st (0 ≤ st < 1000000), the duration d of the order (0 < d < 1000000), and the price p (0 < p < 100000) the customer is ready to pay for this order.

This means that we need to be able to store 10⁶ x 10⁶ x 10⁵ = 10¹⁷ distinct values. An 8 byte signed integer can represent 2⁶³ positive values, and 10¹⁷ < 2⁶³ , so the answer to question 1) is « yes ».

### Encoding an order on a cell

A simple and reliable way to encode 3 distinct values *t*,*d*,*p* in one cell is to multiply these elements by distincts powers of 10 and then add them:<center>*V* = *t* x 10¹¹ + *d* x 10⁵ + *p*</center><br> This can also be written this way:<center>*V* = (*t* x 10⁶ + *d*) x 10⁵ + *p*</center>

Let's write a new word in our script that will do that. It will be our first *colon definition*: 

    : ORDER>CELL ( t,d,p -- n  encode an order in a single cell )
        ROT  1000000 *  ( d,p,t__ )
        ROT + 100000 *  ( p,td_ )
        + ;             ( tdp )

And we can load the script again to test it:

>     gforth Spike.fs ⏎
>     7 140 ORDER>CELL . 300000700140 ⏎ ok 
>     900000 800000 50000 ORDER>CELL . 90000080000050000 ⏎ ok 
>     999999 888888 55555 ORDER>CELL . 99999988888855555 ⏎ ok

###✍
> *`: <name>` start a colon definition for name, compile every following word until `;`*<br>
> *`;` end a semicolon definition and validate the word in the Dictionary*<br>
> *`( ` start a comment and ignore every following word until closing parenthesis*<br>
> *`)` end a comment*<br>

### Decoding a cell into an order

Decoding a cell value into an order, i.e. into 3 values of *start time*, *duration* and *price*, is done according the following formula: <br><center>*t* = *V* / 10¹¹ ; *d* = V % 10¹¹ / 10⁵ ; *p* = V % 10⁵.</center>

Hence the definition:

    : CELL>ORDER ( n -- t,d,p  decode a cell into an order )
        100000  /MOD    ( p,td )
        1000000 /MOD    ( p,d,t )
        SWAP ROT ;      ( t,d,p )

`/MOD` divides the second value on the Stack by the value at the top, and then leaves the modulo and the quotient. 

Let's test our definition:

>     gforth Spike.fs ⏎
>     3 7 140 ORDER>CELL  ⏎  ok 
>     .S <1> 300000700140 ⏎  ok 
>     CELL>ORDER .S <3> 3 7 140 ⏎  ok 

###✍
> *`/MOD ( n,m -- n%m, n/m )` divide n by m, leaving modulo and quotient on the Stack*<br>
> *`SWAP ( a,b -- b,a )` exchange the two values at the top of the Stack*<br>

## Finding the nearest order at a given time
### Comparing orders by start time

The way they are encoded as one single cell, order can still be compared on start time, since
 
<center>*V* = (*t* x 10⁶ + *d*) x 10⁵ + *p*, *V'* = (*t'* x 10⁶ + *d'*) x 10⁵ + *p'*,</center>

<center>*t* > *t'* ⇒ *V* > *V'* </center>
<center>*V* > *V'* ⇒ *t* ≥ *t'* </center>

For example, an order starting at 5 with duration 0 and price 0, is greater than an order starting at 0 with duration 5 and price 100:

>     gforth Spike.fs ⏎
>     5 0 0   ORDER>CELL  ⏎  ok 
>     0 5 100 ORDER>CELL  ⏎  ok 
>     > .  ⏎  -1 ok

### Searching for the nearest order

Let's suppose the `ORDERS` array contains orders sorted by start time, and the last order -- sitting at the position defined by `#ORDERS` is the "maximum" order:

    0 5 100 ORDER>CELL ORDERS !
    3 7 140 ORDER>CELL ORDERS 1 CELLS + !
    5 9  70 ORDER>CELL ORDERS 2 CELLS + !
    3 7  80 ORDER>CELL ORDERS 3 CELLS + !
    2000000 0 0  ORDER>CELL ORDERS 4 CELLS + !
    5 #ORDERS !

To find the nearest order to a given time, we can start at the beginning of the array, compare the content of the cell there with our time; if the time is greater, we increment the address by one cell and repeat the loop, if it's lower or equal, we exit the loop, and we have the address of that order. If all the orders have been compared, then the last order with start time = 2000000 will provoke the exit.

    : NEAREST ( t,addr -- addr  nearest order with start time >= t )
        BEGIN
            OVER OVER   ( t,addr,t,addr ) 
            @ >         ( t,addr,flag )
        WHILE CELL+     ( t,a  )
        REPEAT NIP ;    ( addr )
    
The words `BEGIN`, `WHILE` and `REPEAT` work together in a colon definition. `WHILE` pulls the top of the stack: if it is non zero, the execution continues; if it is zero, the execution jumps after the `REPEAT` word. `REPEAT` make the execution jump to just after the `BEGIN` word.
