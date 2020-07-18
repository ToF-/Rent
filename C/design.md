
## Schedule

    Every order (start time, duration, price) generates 3 events :

        at start time, check the maximum value made so far
        at start time, schedule value that will be made at time start time + duration
        t start time + duration, check the maximum value made so far

    sort the events by time 

    traverse the event table with the following:

        if the event is a check event
            update the current value with the max between current value and value at the time of event
        else if the event is a schedule event
            set the value at time of event with max between current value + price of the event and value at time of event

    after the loop, the current value is the maximum

    finding an event with a specific start time is done by binary search
    there is no need to remove duplicate time points since binary search is deterministic (duplicate events are left unused)

## Stack

    each order has a supplementary value field initialized to NIL (not assigned)

    Sort the orders by start time and duration
    add a sentinel at the end (position MAX) with start time INFINITE and value 0

    push the values 0 and NIL on the stack
    loop while the stack is not empty
        pop the indice N of next order to calculate
        pop the indice I of current order
        J = indice I+1 of following order
        if N is NIL (hasn't been computed yet)
            find N the next order from position I+1, that is compatible with order at I
            push the values I and N

            if J < MAX and V[J] == NIL
                push the values J and NIL

            if N < MAX and V[N] == NIL
                push the values N and NIL
        else (N != NIL)
            set V[I] = max (O[I].p + V[N], V[J])
    at the end, V[0] is the maximum
       
## Backwards

    sort the orders by start time and duration
    add a sentinel at the end (position MAX) with start time INFINITE and value 0

    loop with I from MAX-1 downto 0
        find N the next order from position I+1, that is compatible with order at I
        set O[I].p = max (O[I].p + O[N].p, O[I+1].p)
    at the end, O[0].p is the maximum

