Rent your airplane and make money
=================================

The Problem
-----------

Problem code: [RENT](http://www.spoj.com/problems/RENT/).
 
"ABEAS Corp." is a very small company that owns a single airplane. The customers of ABEAS Corp are large airline companies which rent the airplane to accommodate occasional overcapacity.

Customers send renting orders that consist of a time interval and a price that the customer is ready to pay for renting the airplane during the given time period. Orders of all the customers are known in advance. Of course, not all orders can be accommodated and some orders have to be declined. Eugene LAWLER, the Chief Scientific Officer of ABEAS Corp would like to maximize the profit of the company.

You are requested to compute an optimal solution.

###Small Example

Consider for instance the case where the company has 4 orders:

1. Order 1 (start time 0, duration 5, price 10)
2. Order 2 (start time 3, duration 7, price 8)
3. Order 3 (start time 5, duration 9, price 7)
4. Order 4 (start time 6, duration 9, price 8)

The optimal solution consists in declining Order 2 and 3 and the gain is 10+8 = 18.
Note that the solution made of Order 1 and 3 is feasible (the airplane is rented with no interruption from time 0 to time 14) but non-optimal.

###Input

The first line of the input contains a number T ≤ 30 that indicates the number of test cases to follow. The first line of each test case contains the number of orders n (n ≤ 10000). In the following n lines the orders are given. Each order is described by 3 integer values: The start time of the order st (0 ≤ st < 1000000), the duration d of the order (0 < d < 1000000), and the price p (0 < p < 100000) the customer is ready to pay for this order.

###Output

You are required to compute an optimal solution. For each test case your program has to write the total price paid by the airlines.

Example

###Input:

    1
    4
    0 5 10
    3 7 14
    5 9 7
    6 9 8

###Output:

    18

Warning: large Input/Output data, be careful with certain languages

Why Is This Problem Interesting?
--------------------------------

Because a workable solution for it requires the beginner to learn about:

- traversing a data collection 
- sorting
- mapping (associative array)

Why *Forth*, *C*, *Awk*, *Haskell*?
------------------------

*Forth*
 
- Simplest imperative language
    - small definitions assembled in a bottom-up approach
- Adapting the problem to the language and environment
    - evaluating the input data as part of the source program
    - using the `act` library which provides AVL trees
    - creating compound keys on a single integer word

*C*

- Using classical constructs: array, struct
- Sorting and binary search
- Merging and processing two ordered arrays 

*Awk*

- Combining simple, powerful unix tools reveals
- Regular Expressions
- Associative arrays
- Traversal and breaks

*Haskell*

- Pure functions
- Folding
- Map data structure

Analysing the Problem
---------------------

We can determine the profit of a list of N orders using this formula

- *P = maximum { P(i),P(i+1),..P(n) }*
- where:
    - *P(i) = p(i) + maximum { P(j) | j=1..n, t(j) >= t(i)+d(i) }*
    - t(i),d(i) and p(i) = the start time, duration and price for an order(i)

Applied to the example given in the specification:

- *P = maximum { P(1),P(2),P(3),P(4) }*
- *P(1) = 100 + maximum { P(3),P(4) }*
- *P(1) = 100 + maximum { 80 + maximum { }, 70 + maximum { } }*
- *P(1) = 180*
- *P(2) = 140 + maximum { }*
- *P(3) = 80 + maximum { }*
- *P(4) = 70+ maximum { }*
- *P = 180*

Infortunately this formula is not practical as the number of comparison required by this computation is O(N!).

Another way to compute the solution is to define *P(t)*, the profit value at time *t*, and consider four rules:

1. *P(0) ≥ 0*
2. *∀ t,t'  t'>t ⇒  P(t') ≥ P(t)*
3. *∀ t,d>0,p>0 | order(t,d,p) ⇒ P(t+d) ≥ P(t)+p*
4. *P = P(t) | t = maximum { s+d | order(s,d,p) }*
    
Let's apply those rules to the case given in the request as an example:

- *order(0,5,100) ⇒ P(5)  ≥ P(0)+100* (r.5)
- *P(0) ≥ 0       ⇒ P(5)  ≥ 100* (r.1)
- *order(3,7,140) ⇒ P(10) ≥ P(3)+140*
- *P(3) ≥ P(0)    ⇒ P(10) ≥ 140*
- *order(5,9, 80) ⇒ P(14) ≥ P(5)+80*
- *P(5) ≥ 100     ⇒ P(14) ≥ 180*
- *order(6,9, 70) ⇒ P(15) ≥ P(6)+70*
- *P(6) ≥ P(5)    ⇒ P(15) ≥ 170* (r.2)
- *P(15) ≥ P(14)  ⇒ P(15) ≥ 180* (r.2)
- *P = P(15)      ⇒ P ≥ 180*

Using the four rules suggests the following algorithm for solving our problem:

1. Planning the orders
    - using a planner with a cell for each possible time
    - each cell can contain a value, and notes
    - for each order (t, d, p) :
        - write a note in cell [t]   : {RENT [d] [p]}
        - write a note in cell [t+d] : {CASH}
        
2. Computing the value
    - start with P = 0
    - run through each cell [t] of the planner in chronological order
    - if cell [t] contains a {CASH} note:
        - update P with the value in the cell if the value is greater than P (no value = 0)
    - if cell [t] contains a {RENT [d] [p]} note:
        - in cell [t+d] write the value [P+p] if [P+p] is greater than that the value already at that cell

After that traversal, P will be equal to the maximum profit value for the given plan.

Let's try this algoritm with our example case. First, we plan our orders:

 - order at 0 duration 5 100:
     - note in cell 0 : RENT 5 100
     - note in cell 5 : CASH
 - order at 3 duration 7 140:
     - note in cell 3  : RENT 10 140
     - note in cell 10 : CASH
 - order at 5 duration 9 80:
     - note in cell 5  : RENT 14 80
     - note in cell 14 : CASH
 - order at 6 duration 9 70:
     - note in cell 6  : RENT  15 70
     - note in cell 15 : CASH

The planner should look like this:

    +----------+----------+----------+----------+----------+----------+----------+
    |     0    |     3    |     5    |    6     |    10    |    14    |    15    |
    +----------+----------+----------+----------+----------+----------+----------+
    |         0|         0|         0|         0|         0|         0|         0|
    |RENT 5 100|RENT 7 140|CASH      |RENT 15 70|CASH      |CASH      |CASH      |
    |          |          |RENT 15 80|          |          |          |          |
    +----------+----------+----------+----------+----------+----------+----------+

Then we run through the planner:

- starting with P = 0
- at cell  0: P(5)  ← max(P(5),P+100) = 100
- at cell  3: P(10) ← max(P(10),P+140) = 140
- at cell  5: P ← max(P,P(5)) = 100
- at cell  5: P(14) ← max(P(14),P+80) = 180 
- at cell  6: P(15) ← max(P(15),P+70) = 170 
- at cell 10: P ← max(P,P(10)) = 140 
- at cell 14: P ← max(P,P(14)) = 180 
- at cell 15: P ← max(P,P(15)) = 180 

And P is now equal to the maximum profit value we can draw from the orders: 

            P=0        P=0      P=100      P=100      P=140      P=180      P=180
    +----------+----------+----------+----------+----------+----------+----------+
    |     0    |     3    |     5    |    6     |    10    |    14    |    15    |
    +----------+----------+----------+----------+----------+----------+----------+
    |         0|         0|       100|         0|       140|       180|       170|
    |RENT 5 100|RENT 7 140|CASH      |RENT 15 70|CASH      |CASH      |CASH      |
    |          |          |RENT 15 80|          |          |          |          |
    +----------+----------+----------+----------+----------+----------+----------+

This algorithm can be translated into a program quite simply:

- for each order *(t,d,p)* in the input, create two records of actions to perform :
    - *cash* action at *t+d*
    - *rent* action at *t* to plan until *t+d*, for price *p*
- sort these actions by time then action category (for a given time, perform *cash* action first) 
- starting with *P* = O and an empty plan mapping time to values, traverse the sequence of actions:
    - if *cash (t)* : update *P* with value at *plan[t]*
    - if *rent (t,d,p)* : update *plan[t+d]* with *P+p*

where *update(x,y)* means : the value *x← max (x,y)*

At the end of the traversal, *P* is equal to the maximum profit.
