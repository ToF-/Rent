
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

Let's suppose we have an order given on the stack: it's easy, we just enter a start time, a duration, and a price.

    3 7 140 ⏎ ok

Then we can easily compute and print the end time of the order, i.e. the time at which another order could add some profit:

    ROT ROT + . ⏎ 10 ok

And now the only thing remaining on the stack is the price:

    . ⏎ 140 ok

To understand what happened, let's add a `.S` (print the content of the stack) after each word we type:  

    3 7 140 .S ⏎  <3> 3 7 140  ok
    ROT     .S ⏎  <3> 7 140 3  ok
    ROT     .S ⏎  <3> 140 3 7  ok
    +       .S ⏎  <2> 140 10  ok
    .    .S ⏎  10 <1> 140  ok
    .   .S ⏎  140 <0>  ok

## Using the Dictionary memory

We certainly cannot store all our orders on the stack: that would represent 30000 values for a maximal case, and the stack can store no more than 2040 values. We will have to use the dictionary memory. First let's define some new words for the constants in our program:

    3 CELLS CONSTANT ORDER-SIZE ⏎  ok
    10000   CONSTANT MAX-ORDERS ⏎  ok

The word `CELLS` multiplies the value on the stack by the standard size of a cell in bytes, which is 8 on gforth. Now we can calculate how much memory we will need in the dictionary:

    ORDER-SIZE MAX-ORDERS * .  ⏎  240000 ok

We create a new entry in the dictionary, and allocate as much byte as needed by our array of 10000 orders:

    CREATE ORDERS
    ORDER-SIZE MAX-ORDERS * ALLOT  ⏎  ok

Now `ORDERS` is a new word in our dictionary, and its effect is to put the address of our memory zone on the stack:

    ORDERS . ⏎  4458092120 ok

Finally we need a variable to store the actual number of orders:

    VARIABLE #ORDERS ⏎  ok
    #ORDERS . ⏎  4458332328  ok

Interestingly, if we subtract the last word we created from this one:

    #ORDERS ORDERS - . ⏎  240208 ok

We find the space between the two words to be slighty above the 240000 bytes that we asked for.

## Adding Orders 

Let's use what we have here! Asking the number of orders is easy:

    #ORDERS @ . ⏎  0 ok

Storing another value in that variable, just for the fun:

    42 #ORDERS ! ⏎  ok
    #ORDERS @ .  ⏎  42 ok
    #ORDERS OFF ⏎  ok

(`OFF` is a synonym of: `0 SWAP !`)<br>
Let's put an order, say, 3 7 140, at the first location available in our array, without forgetting to adjust the counter:

    3   ORDERS ! ⏎  ok
    7   ORDERS CELL+ ! ⏎  ok
    140 ORDERS CELL+ CELL+ ! ⏎  ok
    1   #ORDERS +! ⏎  ok

(`CELL+` adds 8 to address on the top of the stack, `+!` is a convenient equivalent of `DUP @ ROT + SWAP !`). Now if we examine the content of the memory around the array `ORDERS` we should see our valuesi (8C is 140 in hexadecimal) :

    ORDERS 24 DUMP ⏎
    109B91658: 03 00 00 00  00 00 00 00 - 07 00 00 00  00 00 00 00  ................
    109B91668: 8C 00 00 00  00 00 00 00 -                           ........
     ok

## New definitions

Storing an order in memory is quite complicated. We can simplify this with new words of our own. 


