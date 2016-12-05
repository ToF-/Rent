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

First we need to be able to store and retrieve values at any `time` position in a table -- let's call this table `PLAN`. These operations follow two rules:

- If a position was never updated before, it's value should be 0.
- A value at a position can be updated only with greater value.

Let's create a file where to put our program:

    \ Rent.fs
    \ Solving the RENT problem in gforth


And then write our first test:

    \ Tests.fs
    REQUIRE ffl/tst.fs
    REQUIRE Rent.fs

    T{
    ." PLAN value (eg at 42) is O by default" CR
    42 PLAN [@] ?0
    }T
    BYE

In this test, we are supposing that two words: `PLAN` and  `[@]` will respectively put the address of our table on the stack, and fetch the value at position 42 in this table.

Of course this doesn't work:

    Forth/Tests.fs:6: Undefined word
        42 >>>PLAN<<< []  ?0

That's because the `PLAN` definittion doesn't exist yet. 
How should we implement such a table ? 
Let's start with a simple proof of concept. We will pretend for a moment that the *start time* can only be comprised betmeen 0 and 100, as well as *duration*. That means that the maximum time value is 200. This allow for our `PLAN` to reside in the dictionnary:

    \ Rent.fs
    \ Solving the RENT problem in gforth

    CREATE PLAN  200 CELLS ALLOT  PLAN 200 CELLS ERASE   

Now the test crashes for the *other* reason:

    Forth/Tests.fs:6: Undefined word
        42 PLAN >>>[@]<<<  ?0

Let's define `[@]` that will access the table:

    \ Rent.fs
    \ Solving the RENT problem in gforth

    CREATE PLAN  200 CELLS ALLOT  PLAN 200 CELLS ERASE   

    : [@]  ( i t -- n  retrieve value at position i in table t or 0 )
       CELLS + @ ;

And the test passes.

Now we can define a word to update our table. Let's write a test first:

    REQUIRE ffl/tst.fs
    REQUIRE Rent.fs

    T{
    ." PLAN value (eg at 42) is O by default" CR
    42 PLAN [@]  ?0

    ." after an update, the value can be retrieved" CR
    4807 42 PLAN [!]
    42 PLAN []  4807 ?S
    }T
    BYE

Of course it fails:

    Forth/Tests.fs:9: Undefined word
        4807 42 PLAN >>>[!]<<<

Let's write a definition for `[!]`:

    \ Rent.fs
    \ Solving the RENT problem in gforth

    CREATE PLAN  200 CELLS ALLOT  PLAN 200 CELLS ERASE   

    : [@]  ( i t -- n  retrieve value at position i in table t or 0 )
       SWAP CELLS + @ ;

    : [!] ( n i t -- update value at position i in table t with n )
        SWAP CELLS + ! ;

Let's write a third test that will show that a table cannot be updated with a smaller value than the value already present:

    ." updating with a smaller value is not possible" CR
    4807 42 PLAN [!]
    4096 42 PLAN [!]
    42 PLAN [@]  4807 ?S

This test fails:

    updating with a smaller value is not possible
    stack contents mismatch:     42 PLAN []  4807 ?S
      expecting 4807 and found 4096

We have to change our definition so that the current value is read first, and  the table is updated with the maximum between the current and the new value:

    : [@]  ( i t -- n  retrieve value at position i in table t or 0 )
       SWAP CELLS + @ ;

    : [!] ( n i t -- update position i in table t with n if not smaller )
        SWAP CELLS + 
        DUP @
        ROT MAX
        SWAP ! ;             

And now the test passes. We can refactor this code:

    : POS ( i t -- address of position i in table t )
        SWAP CELLS + ;

    : [@]  ( i t -- n  retrieve value at pos i in table t or 0 )
       POS @ ;

    : [!] ( n i t -- update pos i in t with n if greater )
        POS DUP @
        ROT MAX
        SWAP ! ;             
