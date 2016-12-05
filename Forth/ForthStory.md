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

Let's create a file where to put our program:

    \ Rent.fs
    \ Solving the RENT problem in gforth

First we need to be able to retrieve a value at a `time` position in a table that we will name `PLAN`. If that position in table was not updated yet, the value should be 0.

    REQUIRE ffl/tst.fs
    REQUIRE Rent.fs

    T{
    ." PLAN value (eg at 42) is O by default" CR
    42 PLAN []  ?0
    }T
    BYE

Of course this doesn't work:

    Forth/Tests.fs:6: Undefined word
        42 >>>PLAN<<< []  ?0

Let's start with a simple proof of concept. We will pretend for a moment that the *start time* can only be comprised betmeen 0 and 100, as well as *duration*. That means that the maximum time value is 200. This allow for our `PLAN` to reside in the dictionnary:

    \ Rent.fs
    \ Solving the RENT problem in gforth

    CREATE PLAN  200 CELLS ALLOT  PLAN 200 CELLS ERASE   

Now the test crashes for another reason:

    Forth/Tests.fs:6: Undefined word
        42 PLAN >>>[]<<<  ?0

Let's define `[]` that will access the table:

    \ Rent.fs
    \ Solving the RENT problem in gforth

    CREATE PLAN  200 CELLS ALLOT  PLAN 200 CELLS ERASE   

    : CELLS+ ( i t -- adr  compute adress of position i in table t )
        SWAP CELLS + ;

    : []  ( i t -- n  retrieve value of position i in table t or 0 )
       CELLS+ @ ;

Now we can define a word to update our table. Let's write a test first:

    REQUIRE ffl/tst.fs
    REQUIRE Rent.fs

    T{
    ." PLAN value (eg at 42) is O by default" CR
    42 PLAN []  ?0

    ." after updating the value can be retrieved" CR
    4807 42 PLAN [!]
    42 PLAN []  4807 ?S
    }T
    BYE

Of course it fails:

    Forth/Tests.fs:9: Undefined word
        4807 42 PLAN >>>[!]<<<

Here's our definition:

    \ Rent.fs
    \ Solving the RENT problem in gforth

    CREATE PLAN  200 CELLS ALLOT  PLAN 200 CELLS ERASE   

    : CELLS+ ( i t -- adr  compute adress of position i in table t )
        SWAP CELLS + ;

    : []  ( i t -- n  retrieve value of position i in table t or 0 )
       CELLS+ @ ;

    : [!] ( n i t -- update value at position i in table t with n )
        CELLS+ ! ;

The table cannot be updated with a smaller value than the value already present:

    ." updating with a smaller value is not possible" CR
    4807 42 PLAN [!]
    4096 42 PLAN [!]
    42 PLAN []  4807 ?S

This test fails:

    PLAN value (eg at 42) is O by default
    after updating the value can be retrieved
    updating with a smaller value is not possible
    stack contents mismatch:     42 PLAN []  4807 ?S
      expecting 4807 and found 4096

Our word should first read the current, then update the table with the maximum between current and new value: 

    : [!] ( n i t -- update position i in table t with n if not smaller )
        CELLS+ DUP @
        ROT MAX
        SWAP ! ;             

