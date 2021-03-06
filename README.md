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

Why Forth, C, Awk, Haskell?
------------------------

Forth
 
- Simplest imperative language
    - small definitions assembled in a bottom-up approach
- Adapting the problem to the language and environment
    - evaluating the input data as part of the source program
    - using the `act` library which provides AVL trees
    - creating compound keys on a single integer word

C

- Using classical constructs: array, struct
- Sorting and binary search
- Merging and processing two ordered arrays 

Awk

- Combining simple, powerful unix tools reveals
- Regular Expressions
- Associative arrays
- Traversal and breaks

Haskell

- Pure functions
- Folding
- Map data structure

Analysing the Problem
---------------------

We can determine the best profit of a list of N orders using this formula

- B = maximum { B(i),B(i+1),..B(N) }
- where:
    - B(i) = p(i) + maximum { B(j) | j=1..n, t(j) ≥ t(i)+d(i) }
    - t(i),d(i) and p(i) = the start time, duration and price for an order(i)

Applied to the example given in the specification:

- B = maximum { B(1),B(2),B(3),B(4) }
- B(1) = 100 + maximum { B(3),B(4) }
- B(1) = 100 + maximum { 80 + maximum {∅}, 70 + maximum {∅} }
- B(1) = 180
- B(2) = 140 + maximum {∅}
- B(3) = 80 + maximum {∅}
- B(4) = 70+ maximum {∅}
- B = 180

Infortunately this formula is not practical as the number of comparisons required by the computation of B is O(N!).

But if we turn the problem the other way around, and assuming that the sequence of order is sorted by start time, we can see that:

- B(4) = 70 = p(4) given that there is only that order left
- B(3) = 80 = max (p(3),B(4)), given that there are no orders after 4
- B(2) = 140 = max (p(2),B(3)), given that there are no orders after 2 that are compatible with order in 2
- B(1) = 180 = max (p(1)+B(3), B(2)), because 3 is the position of the nearest compatible order to order in 1  

Thus an algorithm to solve this problem is:

    given 
        Order, a structure with t,d,p denote start time, duration, price respectively
        O, an array of orders with indices from 0 to N
    sort the orders by start time
    declare an array B of size N+1, initialized with 0s
    for i ← N-1 to 0
        B[i] ← max(p(O[i]) + B[f(i)], B[i+1])
    where 
        f(i) = minimum(j | j ← i+1..N, t(j) ≥ t(O[i])+d(O[i]) ∨ j==N)
    B[0] = best profit for the sequence of orders.
    
Finding the mimimum j for which t(j) is greater or equal to a given time x can be done by binary search sincte the array O is sorted:

    f(i) = find(i+1,N,t(O[i])+d(O[i]))
    find(l,h,x) {
        while(h-l > 1) {
            m ← (h-l)/2 
            if (t(O[m]) < x)
                l ← m
            else
                h ← m
        }
        return h
    }

With this algorithm, the number of comparisons falls to O(N.log(N))

Proof

    for each order at index i
    T[i] = start time of order i
    D[i] = duration
    P[i] = price 
    V[i] = profit value at order i
    
    for i in {0..N-1}
        V[i] ← 0
    T[N] ← +∞
    V[N] ← 0

    for i in {N-1..0}
        l ← i + 1
        h ← N
        while l ≤ h
            m ← l + (h - l) / 2
            if T[m] < T[i] + D[i]
                l ← m + 1
            else
                k ← m
                h ← m - 1
        V[i] ← max(P[i] + V[k], V[i+1])
    → V[0]

Preconditions

    ∀ i ∈ { 0..N-1 }
        P[i] > 0
        D[i] > 0
        T[i] ≤ T[i+1]

Invariant I(i)

        k = min { j | j > i, T[j] ≥ T[i] + D[i] }
        V[i] = max(P[i] + V[k], V[i+1])

After

    for i in {0..N-1}
        V[i] ← 0
    T[N] ← +∞
    V[N] ← 0
        i ← N-1
        l ← i + 1
        h ← N
        while l ≤ h
            m ← l + (h - l) / 2
            if T[m] < T[i] + D[i]
                l ← m + 1
            else
                k ← m
                h ← m - 1
        V[i] ← max(P[i] + V[k], V[i+1])

I(N-1) is true:

    l = N; h = N; l ≤ h; m = N; k = N
    V[N-1] = max(P[N-1] + 0, 0)

After
        i ← N-2
        l ← i + 1
        h ← N
        while l ≤ h
            m ← l + (h - l) / 2
            if T[m] < T[i] + D[i]
                l ← m + 1
            else
                k ← m
                h ← m - 1
        V[i] ← max(P[i] + V[k], V[i+1])

I(N-2) is true:

    l = N-1; h = N; l ≤ h; m = N-1 
        case where T[N-1] < T[N-2] + D[N-2]:
            l = N; m = N; k = N
            V[N-2] ← max(P[N-2] + V[N], V[N-1])
            V[N-2] ← max(P[N-2], V[N-1])
        case where T[N-1] ≥ T[N-2] + D[N-2]:
            k = N-1; h = N-2
            V[N-2] ← max(P[N-2] + V[N-1], V[N-1])

