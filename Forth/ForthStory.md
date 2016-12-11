#FORTH implementation

Let's first decompose our problem into smaller problems:

1. Storing, retrieving and updating values in a table that will map *time* values to *money* values
2. For each *order* entered, keeping track of two category of *actions* : *{CASH}* or *{RENT}*
3. Sorting the list of *actions* by time then category, and executing all them in that order
4. Reading the standard input and processing each batch of *orders*.

###Writing Tests

We will rely on the [`tst`](http://irdvo.nl/FFL/docs/tst.html) library.

<pre><code style="color:green;font-family:monospace">
\ Tests.fs

REQUIRE ffl/tst.fs
T{
." dummy test" CR
2 3 +  4 ?S
}T
BYE

</code></pre>
Here's what happens when we launch a test that fails:

<pre><code style="color:black;font-family:monospace">
gforth Tests.fs ⏎
dummy test
stack contents mismatch:     2 3 +  4 ?S
  expecting 4 and found 5

</code></pre>

Making the test pass:

<pre><code style="color:green;font-family:monospace">
\ Tests.fs

REQUIRE ffl/tst.fs
T{
." dummy test" CR
2 2 +  4 ?S
}T
BYE

</code></pre>
Here's what happens when we launch a test that passes:

<pre><code style="color:black;font-family:monospace">
gforth Tests.fs ⏎
dummy test

</code></pre>
###Mapping time values to money values

First we need to be able to store and retrieve values at any `time` position in a table that we will call *plan*. These operations follow two rules:

- If a position was never updated before, it's value should be 0.
- A position in the table can be updated only with a greater value.

Let's create a file where to put our program:

<pre><code style="color:blue;font-family:monospace">
\ Rent.fs
\ Solving the RENT problem in gforth

</code></pre>
And then write our first test:

<pre><code style="color:green;font-family:monospace">
\ Tests.fs
REQUIRE ffl/tst.fs
REQUIRE Rent.fs

T{
." values can be stored and retrieved in the plan table" CR
4807 42 PLAN!  42 PLAN@ 4807 ?S
256  17 PLAN!  17 PLAN@ 256  ?S
}T
BYE

</code></pre>
This test describes what happens after we store the value 4807 at position 42 of the plan, then retrieving that same position : 4807 should be on the stack. The same happens with value 256 at position 17.

Of course the test crashes, because the words `PLAN!` and `PLAN@` don't exist yet:
<pre><code style="color:black;font-family:monospace">
Forth/Tests.fs:7: Undefined word
4807 42 >>>PLAN!<<<  42 PLAN@ 4807 ?S

</code></pre>
Let's make the test pass. First we need a table for the plan.
How should we implement such a table ? 
Let's pretend for a moment that the *start time* can only be comprised betmeen 0 and 100, as well as *duration*. That means that the maximum time value is 200. This allow for our `PLAN` to reside in the dictionnary:

<pre><code style="color:blue;font-family:monospace">
\ Rent.fs
\ Solving the RENT problem in gforth

CREATE PLAN  200 CELLS ALLOT  PLAN 200 CELLS ERASE   

</code></pre>
We create a word `PLAN` then reserve 200 CELLS (200 x 8) bytes of memory, then fill this space with zeros.
Now we can write our definitions for retrieving and storing values in the table:

<pre><code style="color:blue;font-family:monospace">
: PLAN@ ( t -- retrieve plan value at time t or 0 )
    CELLS PLAN + @ ;

: PLAN! ( n t -- store value n at time t in plan )
    CELLS PLAN + ! ;

</code></pre>
Now our test pass.

</code></pre>
Here's a new test: the plan cannot be updated with a smaller value than the value already present:

<pre><code style="color:green;font-family:monospace">
." value can be stored only if greater than value present" CR
500 23 PLAN!  250 23 PLAN!  23 PLAN@  500 ?S  

</code></pre>
This test fails:

stack contents mismatch:     500 23 PLAN!  250 23 PLAN!  23 PLAN@  500 ?S  
  expecting 500 and found 250

We have to change our definition so that the current value is read first, and  the table is updated with the maximum between the current and the new value:

<pre><code style="color:blue;font-family:monospace">
: PLAN! ( n t -- stores value n at position t in plan )
    CELLS PLAN + 
    DUP @ 
    ROT MAX
    SWAP ! ;

</code></pre>
And now the test passes. Let's remove some repetition here:

<pre><code style="color:blue;font-family:monospace">
: PLAN[] ( t -- addr   address of position t in plan )
    CELLS PLAN + ;

: PLAN@ ( t -- retrieve a value at position t from plan or 0 )
    PLAN[] @ ;

: PLAN! ( n t -- stores value n at position t in plan )
    PLAN[] DUP @ 
    ROT MAX
    SWAP ! ;

</code></pre>
### Updating cash, planning rents

Let's continue with profit calculation. These are the rules:

- when performing a *cash* operation at time t, profit is set to the plan value at that position.
- profit cannot be reduced to a smaller value: if the plan value is smaller than profit, profit is unchanged.
- when performing a *rent* operation a time t for duration d and price p, the value at position t+d in plan is set to p + profit.
- plan cannot be reduced to as smaller value: if the value at position t+d is already greater than profit+p, it is left unchanged.

We need a variable called `PROFIT`, and a word that will put profit and plan to zero. Let's write a test:

<pre><code style="color:green;font-family:monospace">
." initialize reset profit and plan to zeros" CR
4807 PROFIT !  100 42 PLAN!
INITIALIZE
PROFIT @ ?0
42 PLAN@ ?0

</code></pre>
Now let's make the test pass:

<pre><code style="color:blue;font-family:monospace">
VARIABLE PROFIT

: INITIALIZE ( -- put profit and plan to zero )
    0 PROFIT !
    PLAN 200 CELLS ERASE ;
 
</code></pre>
Next test: when we update cash at a given time, profit should increase:

<pre><code style="color:green;font-family:monospace">
." when updating cash, profit should increase" CR
INITIALIZE
4807 42 PLAN!
42 CASH
PROFIT @ 4807 ?S

</code></pre>
We create a new definition:

<pre><code style="color:blue;font-family:monospace">
: CASH ( t -- update profit from plan at a given time )
    PLAN@ PROFIT ! ; 

</code></pre>
Profit should not decrease if the cash value is lower:

<pre><code style="color:green;font-family:monospace">
." when updating cash, profit should not decrease" CR
INITIALIZE
4807 42 PLAN!
4096 43 PLAN!
42 CASH
43 CASH
PROFIT @ 4807 ?S

</code></pre>
To make this test past, we change the definition to keep the maximum value between current profit and the value found in the plan

<pre><code style="color:blue;font-family:monospace">
: CASH ( t -- update profit from plan at a given time )
    PLAN@ 
    PROFIT @ MAX
    PROFIT ! ;

</code></pre>
Planning a rent at a given time for a given duration and price, amounts to:

- update the current profit value at the *start time*
- update the plan at the end time with the value of *profit* + *price*

Let's write a test:
<pre><code style="color:green;font-family:monospace">
." planning rent t d p update plan at t+d with profit + p" CR
INITIALIZE
500 10 PLAN!
10 7 450  RENT
17 PLAN@  950 ?S
  
</code></pre>
Our definition of `RENT` should update profit, add it to the price then update the plan at the end time:

<pre><code style="color:blue;font-family:monospace">
: RENT ( t d p -- update plan according to rent )
    ROT DUP CASH   
    SWAP PROFIT @ + 
    -ROT + PLAN! ;

</code></pre>
Now we can verify our algorithm. By performing `CASH` and `RENT` operations in sequence, we obtain the maximum profit:

<pre><code style="color:green;font-family:monospace">
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

</code></pre> 
And this test passes. It works!

using an AVL tree
-----------------

The specs for the requested program mention that time values can be as large as 2000000, so storing the PLAN table in the dictionary won't work. Try the following in gforth:

<pre><code style="color:black;font-family:monospace">
CREATE PLAN 2000000 CELLS ALLOT
:2: Dictionary overflow
CREATE PLAN 2000000 CELLS >>>ALLOT<<<
Backtrace:
$10568E8E0 throw

</code></pre> 
Besides, using such large dictionary space for only 10000 time point entries only would be wasteful.

Enters [`act`](http://irdvo.nl/FFL/docs/act.html) , a module from the Forth Foundation Library. This module provides us with the ability to store key/values in AVL trees. Here we use `ACT-CREATE` to create a new AVL tree named `PLAN`, and then rewrite our access words so that they use `ACT-INSERT` to insert a new profit node, and `ACT-GET` to retrieve a profit at a given time :


<pre><code style="color:blue;font-family:monospace">
REQUIRE ffl/act.fs

ACT-CREATE PLAN 

: PLAN@ ( t -- retrieve a value at position t from plan or 0 )
    PLAN ACT-GET 0= IF 0 THEN ;

: PLAN! ( n t -- stores value n at position t in plan )
    DUP PLAN@
    ROT MAX 
    SWAP PLAN ACT-INSERT ;

VARIABLE PROFIT

: INITIALIZE ( -- put profit and plan to zero )
    0 PROFIT !
    PLAN ACT-INIT ;

</code></pre> 
Now we can verify that our algorithm also works with values much larger than 200:

<pre><code style="color:green;font-family:monospace">
." maximize profit from cash and rent operations" CR
INITIALIZE
0      50000  100 RENT
30000  70000  140 RENT
50000             CASH
50000  90000  80  RENT
60000  90000  70  RENT
100000            CASH
140000            CASH
150000            CASH
PROFIT @  180 ?S
</code></pre> 
And this test passes, which proves that our AVL tree can handle large keys.

Traversing an AVL tree
----------------------
One very useful feature of `act` library is the ability to execute a given definition on each and every node of a given tree. The execution sequence is sorted by key. Let's try it with gforth:

<pre><code style="color:black;font-family:monospace">
REQUIRE ffl/act.fs
ACT-CREATE TREE

100 5  TREE ACT-INSERT
140 10 TREE ACT-INSERT
180 14 TREE ACT-INSERT
170 15 TREE ACT-INSERT

: PRINT ( value key -- print key and value )
    . ." -->" . CR ;

' PRINT  \ address of the word PRINT
CR TREE ACT-EXECUTE ⏎
5 -->100
10 -->140
14 -->180
15 -->170
 ok

</code></pre> 

Actions key and data
--------------------

Our program requires that Cash and Rent actions are executed in order. A Cash action at a given time should be performed before any Rent action starting at the same time. Hence we must sort the sequence of actions according to 2 criteria:
    - time of action
    - action category, which can be determined by the duration: if duration is 0, then the action is a Cash action, else a Rent action

On the other hand, an AVL tree can store nodes that associate a (1 cell long) key value to a (1 cell long) data value. Each key must be unique in the tree. If we can create a compound key with the time and duration of action, and store each action this way, then the AVL tree can sort the sequence of actions for us. Here's an example:

    action          key         data

    0 5 100 Rent    (0,5)       100
    3 7 140 Rent    (3,7)       140
    5 Cash          (5,0)       --
    5 9 80 Rent     (5,9)       80
    6 9 70 Rent     (6,9)       70
    10 Cash         (10,0)      --
    14 Cash         (14,0)      --
    15 Cash         (15,0)      --

The word:

    act-insert ( x1 x2 act -- )
        Insert data x1 with key x2 in the tree

can be used to store an action in a tree, if we manage to store both time and duration on a 1 cell key. In order to do that, we could:

1. multiply action time by 1000000 (since duration is < 1000000)
2. add duration to this key.

Let's write a test:


<pre><code style="color:green;font-family:monospace">
." an action can be encoded in a key that is ordered" CR
4807 0000 ACTION>KEY
4807 1000 ACTION>KEY
< ?TRUE

</code></pre> 

The word `ACTION-KEY` is very simple:

<pre><code style="color:blue;font-family:monospace">
: ACTION>KEY ( t d -- k  encode time and duration into an action key )
    SWAP 1000000 * + ;

</code></pre> 
Of course we will need to decode a key into an action time and duration:  
<pre><code style="color:green;font-family:monospace">
." a key can be decoded into action time and duration" CR
4807 42 ACTION>KEY
KEY>ACTION  42 ?S  4807 ?S 

</code></pre> 
This word is even simpler:
<pre><code style="color:blue;font-family:monospace">
: KEY>ACTION ( k -- t d  decode a key into an action time and duration )
    1000000 /MOD SWAP ;

</code></pre> 

Now we can write the definitions that will allows us to prepare actions for encoding:
<pre><code style="color:green;font-family:monospace">
." action data and key for a cash action to be stored are 0 and (t,0)" CR
4807 {CASH} 
KEY>ACTION  0  ?S  4807 ?S  
NIL ?S

</code></pre> 


<pre><code style="color:blue;font-family:monospace">
: {CASH} ( t -- d k  prepare data and key for a cash action to be stored )
    0 ACTION>KEY NIL SWAP ; 

</code></pre> 

<pre><code style="color:green;font-family:monospace">
." action data and key for a rent action to be stored are p and (t,d)" CR
4807 42 100 {RENT} 
KEY>ACTION  42  ?S  4807 ?S  
100 ?S

</code></pre> 
<pre><code style="color:blue;font-family:monospace">
: {RENT} ( t d p -- d k  prepare data and key for a rent action to be stored )
    -ROT ACTION>KEY  ; 

</code></pre> 
Storing Actions
---------------

We are almost ready to store actions. Let's write a test:
<pre><code style="color:green;font-family:monospace">
." adding an order stores a rent action at (t,d) and a cash action at (t+d,0)" CR
INIT-ACTIONS
4807 42 100 ADD-ORDER
4807 42 ACTION>KEY ACTIONS ACT-HAS? ?TRUE
4849 0  ACTION>KEY ACTIONS ACT-HAS? ?TRUE 

</code></pre> 
And here is the definition of `ADD-ORDER`:
<pre><code style="color:blue;font-family:monospace">
: ADD-ORDER ( t d p -- stores rent and cash actions for order )
    -ROT 2DUP
    + {CASH} ACTIONS ACT-INSERT 
    ROT {RENT} ACTIONS ACT-INSERT ;

</code></pre> 
This definition works quite well except that when we insert a rent action that is already in the tree, we have to make sure we are not inserting an action for a *smaller price*:
." inserting an order with same time and duration is allowed only for a better price" CR
<pre><code style="color:green;font-family:monospace">
INIT-ACTIONS
4807 42 100 ADD-ORDER
4807 42 ACTION>KEY ACTIONS ACT-GET DROP 100 ?S
4807 42 50 ADD-ORDER
4807 42 ACTION>KEY ACTIONS ACT-GET DROP 100 ?S
4807 42 200 ADD-ORDER
4807 42 ACTION>KEY ACTIONS ACT-GET DROP 200 ?S

</code></pre> 
And this test fails of course:
<pre><code style="color:black;font-family:monospace">
stack contents mismatch:         4807 42 ACTION>KEY ACTIONS ACT-GET DROP 100 ?S
  expecting 100 and found 50

</code></pre> 
We must do a search before inserting, and if the key exists, we must take the `MAX` of the stored value with the new value:
<pre><code style="color:blue;font-family:monospace">
: ACT-UPDATE ( d k tree -- update tree only if k not present or d is greater )
    2DUP ACT-GET 
    IF >R ROT R> MAX -ROT THEN
    ACT-INSERT ;

: ADD-ORDER ( t d p -- stores rent and cash actions for order )
    -ROT 2DUP
    + {CASH} ACTIONS ACT-INSERT 
    ROT {RENT} ACTIONS ACT-UPDATE ;

</code></pre> 
Now we can make our sample test pass:

<pre><code style="color:green;font-family:monospace">
." adding orders then computing profit maximizes profit" CR
INIT-ACTIONS
0 5 100 ADD-ORDER
3 7 140 ADD-ORDER
5 9 80  ADD-ORDER
6 9 70  ADD-ORDER
CALC-PROFIT
PROFIT @  180 ?S

</code></pre> 
It works, but the code lacks simplicity. Interestingly, the rules for retrieving and updating nodes in the `ACTIONS` tree:

1. if a time point is not yet in the plan, the value for this time is 0
2. a value in the plan can be updated only with a greater value

are the same as in the `PLAN` tree:

1. if an action for a time and duration is not yet in the list, the price is considered to be 0
2. an action for a given time and duration in the list can be updated only with a greater price value

So we might as well use the same logic for the `ACTIONS` tree than for the `PLAN` tree:

<pre><code style="color:blue;font-family:monospace">
: PLAN@ ( t -- retrieve a value at position t from plan or 0 )
    PLAN ACT-GET 0= IF 0 THEN ;

: PLAN! ( n t -- stores value n at position t in plan )
    DUP PLAN@
    ROT MAX 
    SWAP PLAN ACT-INSERT ;

: ACTION@ ( k -- d   retrieve action duration or 0 if not found )
    ACTIONS ACT-GET 0= IF 0 THEN ;

: ACTION! ( d k -- update action tree if k not present or d is greater )
    DUP ACTION@
    ROT MAX
    SWAP ACTIONS ACT-INSERT ;

: ADD-ORDER ( t d p -- stores rent and cash actions for order )
    -ROT 2DUP + {CASH} ACTION! 
    ROT         {RENT} ACTION! ;

</code></pre> 
Then we can factor these definitions:
<pre><code style="color:blue;font-family:monospace">
: ACT-@ ( k t -- n  retrieve a k in tree t or 0 )
    ACT-GET 0= IF 0 THEN ;

: ACT-! ( n k t -- store value n at position k in tree t if n is greater )
    2DUP ACT-@ ?DUP IF
        >R ROT R> MAX -ROT 
    THEN ACT-INSERT ;

: PLAN@ ( t -- retrieve a value at position t from plan or 0 )
    PLAN ACT-@ ;

: PLAN! ( n t -- stores value n at position t in plan )
    PLAN ACT-! ;

: ACTION@ ( k -- d   retrieve action duration or 0 if not found )
    ACTIONS ACT-@ ;

: ACTION! ( d k -- update action tree if k not present or d is greater )
    ACTIONS ACT-! ;

</code></pre> 
And even shorten program a bit more by removing these 4 specialized words, calling directly our general words:
<pre><code style="color:blue;font-family:monospace">
: CASH ( t -- update profit from plan at a given time )
    PLAN ACT-@ 
    PROFIT @ MAX
    PROFIT ! ;

: RENT ( t d p -- update plan according to rent )
    ROT DUP CASH   
    SWAP PROFIT @ + 
    -ROT + PLAN ACT-! ;

: ADD-ORDER ( t d p -- stores rent and cash actions for order )
    -ROT 2DUP + {CASH} ACTIONS ACT-! 
    ROT         {RENT} ACTIONS ACT-! ;

</code></pre> 
Computing profit
----------------
The `CALC-PROFIT` definition consists in traversing the sequence of actions ordered by time and category.
Here is the logic to perform for each action:

- if the action is a rent (duration is not zero) :
    perform a `RENT` action with duration and price
- if the action is a cash, perform a `CASH` (and get rid of data)

<pre><code style="color:blue;font-family:monospace">
: PERFORM-ACTION ( d k -- perform the cash or rent action )
    KEY>ACTION ?DUP IF ROT RENT ELSE CASH DROP THEN ;

' PERFORM-ACTION CONSTANT EXEC

: CALC-PROFIT ( -- compute profit for orders added )
    INITIALIZE
    EXEC ACTIONS ACT-EXECUTE ;
:</code></pre> 

Reading from the input stream
-----------------------------

Reading from the standard input is easy. Here's a word that does that:
<pre><code style="color:blue;font-family:monospace">
4096 CONSTANT MAX-LINE

: GET-STDIN ( -- n f  read from stdin on pad, leaving lenght and flag )
    PAD MAX-LINE STDIN READ-LINE THROW ;
     
</code></pre> 
Here's an example of use:
<pre><code style="color:black;font-family:monospace">
GET-STDIN ⏎
Time flies like an arrow ⏎  \ this line is entered at the prompt of GET-STDIN
ok
.S ⏎
 <2> 24 -1  ok
CR DROP PAD SWAP TYPE ⏎
Time flies like an arrow ok
     
</code></pre> 

The simplest way to proceed with the problem input line is to read them and interpret them as if they were gforth instructions. Here's the word to do that:

<pre><code style="color:blue;font-family:monospace">
: EVAL-LINE ( -- read a line from stdin and evaluate it or leave 0 )
    GET-STDIN IF PAD SWAP EVALUATE ELSE 0 THEN ;
     
</code></pre> 
Here's an exemple of input evaluation:
<pre><code style="color:black;font-family:monospace">
EVAL-LINE ⏎
: SQUARE DUP * ; ⏎  \ this line is entered at the prompt
ok 
DROP CR 42 SQUARE . ⏎
1764 ok
     
</code></pre> 

Now that we can evaluate the lines from the standard input, we can get several orders from there. We first read the number of orders, then loop as many times as specified, reading orders and adding them to the tree. 

<pre><code style="color:blue;font-family:monospace">
: GET-ORDERS ( -- read orders from stdin and add them )
    INIT-ACTIONS
    EVAL-LINE 0 DO 
        EVAL-LINE ADD-ORDER
    LOOP ;
     
</code></pre> 

The program is almost complete. We need a word that will read the number of cases, and for each case, read the orders, then compute and print the profit.
<pre><code style="color:blue;font-family:monospace">
: MAIN ( -- read cases from stdin, compute and print profit )
    EVAL-LINE 0 DO
        GET-ORDERS
        INITIALIZE
        CALC-PROFIT
        PROFIT ? CR
    LOOP ;
     
</code></pre> 
To run the program, launch gforth at the prompt, execute our `MAIN`  and then quit gforth:

    echo "1
    4
    0 5 100
    3 7 140
    5 9 80
    6 9 70" >sample.dat

    gforth Rent.fs -e "MAIN BYE" <sample.dat
    180

And we are done!
     
