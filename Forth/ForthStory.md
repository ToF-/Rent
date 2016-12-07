#FORTH implementation

Let's first decompose our problem into smaller problems:

1. Storing, retrieving and updating values in a table that will map *time* values to *money* values
2. For each *order* entered, keeping track of two category of *actions* : *{CASH}* or *{RENT}*
3. Sorting the list of *actions* by time then category, and executing all them in that order
4. Reading the standard input and processing each batch of *orders*.

###Writing Tests

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

###Mapping time values to money values

First we need to be able to store and retrieve values at any `time` position in a table that we will call *plan*. These operations follow two rules:

- If a position was never updated before, it's value should be 0.
- A position in the table can be updated only with a greater value.

Let's create a file where to put our program:

    \ Rent.fs
    \ Solving the RENT problem in gforth


And then write our first test:

    \ Tests.fs
    REQUIRE ffl/tst.fs
    REQUIRE Rent.fs

    T{
    ." values can be stored and retrieved in the plan table" CR
    4807 42 PLAN!  42 PLAN@ 4807 ?S
    256  17 PLAN!  17 PLAN@ 256  ?S
    }T
    BYE

This test describes what happens after we store the value 4807 at position 42 of the plan, then retrieving that same position : 4807 should be on the stack. The same happens with value 256 at position 17.

Of course the test crashes, because the words `PLAN!` and `PLAN@` don't exist yet:

Forth/Tests.fs:7: Undefined word
    4807 42 >>>PLAN!<<<  42 PLAN@ 4807 ?S

Let's make the test pass. First we need a table for the plan.
How should we implement such a table ? 
Let's pretend for a moment that the *start time* can only be comprised betmeen 0 and 100, as well as *duration*. That means that the maximum time value is 200. This allow for our `PLAN` to reside in the dictionnary:

    \ Rent.fs
    \ Solving the RENT problem in gforth

    CREATE PLAN  200 CELLS ALLOT  PLAN 200 CELLS ERASE   

We create a word `PLAN` then reserve 200 CELLS (200 x 8) bytes of memory, then fill this space with zeros.
Now we can write our definitions for retrieving and storing values in the table:

    : PLAN@ ( t -- retrieve plan value at time t or 0 )
        CELLS PLAN + @ ;

    : PLAN! ( n t -- store value n at time t in plan )
        CELLS PLAN + ! ;

Now our test pass.

Here's a new test: the plan cannot be updated with a smaller value than the value already present:

    ." value can be stored only if greater than value present" CR
    500 23 PLAN!  250 23 PLAN!  23 PLAN@  500 ?S  

This test fails:

    stack contents mismatch:     500 23 PLAN!  250 23 PLAN!  23 PLAN@  500 ?S  
      expecting 500 and found 250

We have to change our definition so that the current value is read first, and  the table is updated with the maximum between the current and the new value:

    : PLAN! ( n t -- stores value n at position t in plan )
        CELLS PLAN + 
        DUP @ 
        ROT MAX
        SWAP ! ;

And now the test passes. Let's remove some repetition here:

    : PLAN[] ( t -- addr   address of position t in plan )
        CELLS PLAN + ;

    : PLAN@ ( t -- retrieve a value at position t from plan or 0 )
        PLAN[] @ ;

    : PLAN! ( n t -- stores value n at position t in plan )
        PLAN[] DUP @ 
        ROT MAX
        SWAP ! ;

### Updating cash, planning rents

Let's continue with profit calculation. These are the rules:

- when performing a *cash* operation at time t, profit is set to the plan value at that position.
- profit cannot be reduced to a smaller value: if the plan value is smaller than profit, profit is unchanged.
- when performing a *rent* operation a time t for duration d and price p, the value at position t+d in plan is set to p + profit.
- plan cannot be reduced to as smaller value: if the value at position t+d is already greater than profit+p, it is left unchanged.

We need a variable called `PROFIT`, and a word that will put profit and plan to zero. Let's write a test:

    ." initialize reset profit and plan to zeros" CR
    4807 PROFIT !  100 42 PLAN!
    INITIALIZE
    PROFIT @ ?0
    42 PLAN@ ?0

Now let's make the test pass:

    VARIABLE PROFIT

    : INITIALIZE ( -- put profit and plan to zero )
        0 PROFIT !
        PLAN 200 CELLS ERASE ;
 
Next test: when we update cash at a given time, profit should increase:

    ." when updating cash, profit should increase" CR
    INITIALIZE
    4807 42 PLAN!
    42 CASH
    PROFIT @ 4807 ?S

We create a new definition:

    : CASH ( t -- update profit from plan at a given time )
        PLAN@ PROFIT ! ; 

Profit should not decrease if the cash value is lower:

    ." when updating cash, profit should not decrease" CR
    INITIALIZE
    4807 42 PLAN!
    4096 43 PLAN!
    42 CASH
    43 CASH
    PROFIT @ 4807 ?S

To make this test past, we change the definition to keep the maximum value between current profit and the value found in the plan

    : CASH ( t -- update profit from plan at a given time )
        PLAN@ 
        PROFIT @ MAX
        PROFIT ! ;

Planning a rent at a given time for a given duration and price, amounts to:

- update the current profit value at the *start time*
- update the plan at the end time with the value of *profit* + *price*

Let's write a test:

    ." planning rent t d p update plan at t+d with profit + p" CR
    INITIALIZE
    500 10 PLAN!
    10 7 450  RENT
    17 PLAN@  950 ?S
      
Our definition of `RENT` should update profit, add it to the price then update the plan at the end time:

    : RENT ( t d p -- update plan according to rent )
        ROT DUP CASH   
        SWAP PROFIT @ + 
        -ROT + PLAN! ;

Now we can verify our algorithm. By performing `CASH` and `RENT` operations in sequence, we obtain the maximum profit:

    ." maximize profit from cash and rent operations" CR
    INITIALIZE
    0 5 100 RENT
    3 7 140 RENT
    5       CASH
    5 9 80  RENT
    6 9 70  RENT
    10      CASH
    14      CASH
    15      CASH
    PROFIT @  180 ?S

And this test passes. 
