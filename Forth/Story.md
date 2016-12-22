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


## Using the Stack memory

Let's suppose we have an order given on the Stack: it's easy, we just enter a start time, a duration, and a price.

    3 7 140 ⏎ ok

Then we can easily compute and print the end time of the order, i.e. the time at which another order could add some profit:

    ROT ROT + . ⏎ 10 ok

And now the only thing remaining on the Stack is the price:

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

Then we must *reserve* the right amount of memory bytes to store our 10000 orders. How much should that be? Each order will occupy 3 integer values -- called *cells* -- and we will have 10000 orders:

    3 CELLS MAX-ORDERS * ALLOT  ⏎ ok

The word `CELLS` multiplies the value on the Stack by the standard size of a cell in bytes, which is 8 on gforth. 

Now `ORDERS` is a new word in the Dictionary, and its effect is to put the address of our memory zone, which is also the address of the first order in the array, on the Stack:

    ORDERS . ⏎  4458092120 ok

Let's create another entry: we need another array of 10001 integers, representing the best profit for each order (plus an initial zero value). Let's call this new array `PROFITS`:

    CREATE PROFITS MAX-ORDERS 1+ ALLOT ⏎ ok
    PROFITS . ⏎  4458332160 ok

Now if subtract the `PROFITS` address from the `ORDERS` address:

    PROFITS ORDERS - . ⏎ 240040

We find the difference to be slighty above 240000 bytes, which the the size that we alloted for the `ORDERS` entry.

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
    CREATE ORDERS 3 CELLS MAX-ORDERS * ALLOT
    CREATE PROFITS MAX-ORDERS 1+ ALLOT

And we can launch __gforth__ with this script:

>     11:30:35 ~/dev/Rent/Forth:gforth Spike.fs ⏎
>     Gforth 0.7.2, Copyright (C) 1995-2008 Free Software Foundation, Inc.
>     Gforth comes with ABSOLUTELY NO WARRANTY; for details type `license'
>     Type `bye' to exit
>     PROFITS ORDERS - . 240040  ok
>     #ORDERS ? 0  ok
