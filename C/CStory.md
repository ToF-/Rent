Solving our problem in C will involve very few tools and libraries. We need a way to read the input and print results of course, and computing profit will be done using solely sorting and binary search. 

The algorithm to follow for each case in the input is described below:

initialization:

- read all orders in an array *Rent*  ( *start* = *order.start_time*, *end* = *start+order.duration*, *price* )
- initialize an array of all time points given by each *Rent* start and end times 
- sort the array of time points
- initialize an array *Plan* of cells given by each unique time point ( *time*, *value* )
- sort the array *Rent* in ascending order of start time

computation:

- *Profit* = 0
- current rent index *r* = 0
- current plan index *p* = 0
- while (*p* < size of *Plan*)
    - *Profit* = max(*Profit*, *Plan[p].value*)
    - if (*r* < size of *Rent* and *Rent[r].start* <= *Plan[p].time*)
       - find in *Plan* from (*p*+1) to (size of *Plan* - 1), the position *e* such that *Plan[e].time* = *Rent[r].end*
       - *Plan[e].value* = max(*Plan[e].value*, *Profit* + *Rent[r].price*)
       - *r* = *r* + 1
    - else
       -   *p* = *p* + 1

Setting up a test harness
-------------------------

As we want to buid the program the TDD way, we need to set up a test harness. The idea here is to run the program on a given input file that will include a growing number of cases, and then to `diff` the output with another file containing the expected results. Here's the Makefile, reflecting that idea:

    rent : rent.c
        cc rent.c -o rent

    test : rent test.dat expected.dat
        ./rent <test.dat >result.dat; diff expected.dat result.dat

If we define a file named `test.dat`:
 
    1
    1
    0 5 100

and a file named `expected.dat`:

    100

then the test will not pass until our program outputs the value `100`.

Let's write a first version of `rent.c` that will make the test fail:

    #include <stdio.h>

    int main() {
        printf("%d\n", 0);
        return 0;
    }

`make test` results in:

    ./rent <test.dat >result.dat; diff expected.dat result.dat
    1c1
    < 100
    ---
    > 0
    make: *** [test] Error 1

    shell returned 2

Let's make our program pass real quick:

    #include <stdio.h>

    int main() {
        printf("%d\n", 100);
        return 0;
    }

And now the test pass:

    ./rent <test.dat >result.dat; diff expected.dat result.dat

as shown by the absence of any error message from `make`.

This test harness can be improved so that it doesn't require the updating of two separate files. Instead, we can have a `tests.txt` file describe our tests:

    Tests for rent.c
    # cases
    < 1
    A single order should result in that order price
    < 1
    < 0 5 100

    > 100

and ask the O.S to split that file and strip the text part, using the `< ` and `> ` prefixes as markers for lines that should go into `test.dat` and `expected.dat` files respectively. Using *sed* in our `Makefile` will do the trick:

    rent : rent.c
        cc rent.c -o rent

    test : rent tests.txt
        sed -n -e 's/\(< \)\(.*\)/\2/pw test.dat'     tests.txt >/dev/null
        sed -n -e 's/\(> \)\(.*\)/\2/pw expected.dat' tests.txt >/dev/null
        ./rent <test.dat >result.dat
        diff expected.dat result.dat

Reading several cases
---------------------

Of course our first version is widely incomplete. The program should at least process different orders with different prices. Let's add a test:

    Tests for rent.c
    # cases
    < 2
    A single order should result in that order price
    < 1
    < 0 5 100
    > 100
    A different order price should make a different profit
    < 1
    < 0 5 110
    > 110

the test fails:

    2d1
    < 110
    make: *** [test] Error 1

    shell returned 2

Here's a very ugly, quick and dirty way to make it pass:

    #include <stdio.h>
    #define MAXLINE 4096

    char line[MAXLINE];

    int main() {
        fgets(line, MAXLINE, stdin);
        fgets(line, MAXLINE, stdin);
        int start_time; int duration; int price;
        fgets(line, MAXLINE, stdin);
        sscanf(line, "%d %d %d", &start_time, &duration, &price); 
        printf("%d\n", price);
        fgets(line, MAXLINE, stdin);
        fgets(line, MAXLINE, stdin);
        sscanf(line, "%d %d %d", &start_time, &duration, &price); 
        printf("%d\n", price);
        return 0;
    }

Let's refactor this into something decent:

    #include <stdio.h>
    #define maxline 4096

    char line[maxline];

    char *get_line(char *line) {
        fgets(line, maxline, stdin);
        return line;
    }

    int get_int(char *line) {
        int result;
        sscanf(get_line(line), "%d", &result);
        return result;
    }

    int main() {
        int max_cases = get_int(line);
        for(int c = 0; c < max_cases; c++) {
            get_line(line); // read #orders, not used yet
            int start_time; int duration; int price;
            sscanf(get_line(line), "%d %d %d", &start_time, &duration, &price); 
            printf("%d\n", price);
        }
    return 0;
    }

Reading cases with several orders
---------------------------------
Let's move further. If a case contains several orders, and they are all incompatible, then the profit made is equal to the greater price:

    Tests for rent.c
    # cases
    < 3
    A single order should result in that order price
    < 1
    < 0 5 100
    > 100
    A different order price should make a different profit
    < 1
    < 0 5 110
    > 110
    With several incompatible orders profit should equal the best price
    < 3
    < 0 5 100
    < 3 7 120
    < 4 3  90
    > 120

To make this test pass, we read all the orders in the case, and keep the maximum price:

    int main() {
        int max_cases = get_int(Line);
        for(int c = 0; c < max_cases; c++) {
            int max_orders = get_int(Line);
            int profit = 0;
            for(int o = 0; o < max_orders; o++) {
                int start_time; int duration; int price;
                sscanf(get_line(Line), "%d %d %d", &start_time, &duration, &price); 
                if(price > profit)
                    profit = price;
            }
            printf("%d\n", profit);
        }
        return 0;
    }

Computing profit with a sorted order sequence
---------------------------------------------
What should be the next step? If the orders are already sorted by ascending start time, and the number of time points doesn't exceed a certain range, we can implement a naive version of the algorithm. Let's write a new test first:

    Tests for rent.c
    # cases
    < 4
    A single order should result in that order price
    < 1
    < 0 5 100
    > 100
    A different order price should make a different profit
    < 1
    < 0 5 110
    > 110
    With several incompatible orders profit should equal the best price
    < 3
    < 0 5 100
    < 3 7 120
    < 4 3  90
    > 120
    Given ordered orders in a small range on distinct times, profit should be optimal
    < 4
    < 0 5 100
    < 3 7 140
    < 5 9 80
    < 6 9 70
    > 180

The naive algorithm is quite simple: for each order, update the profit at the start time of the order. then plan an increase in profit by the price, updating the value at the end time of the order. Then traverse the plan in search of the maximum value.

    #define MAXTIME 20
    int Plan[MAXTIME];

    int main() {
        int max_cases = get_int(Line);
        for(int c = 0; c < max_cases; c++) {
            for(int i = 0; i < MAXTIME; i++)
                Plan[i] = 0;
            int max_orders = get_int(Line);
            int profit = 0;
            for(int o = 0; o < max_orders; o++) {
                int start_time; int duration; int price;
                sscanf(get_line(Line), "%d %d %d", &start_time, &duration, &price); 
                if(Plan[start_time] > profit)
                    profit = Plan[start_time];
                Plan[start_time + duration] = profit + price;
            }
            for(int t = 0; t < MAXTIME; t++) 
                if(Plan[t] > profit)
                    profit = Plan[t];
            printf("%d\n", profit);
        }
        return 0;
    }

Updating planned profit with only greater values
------------------------------------------------
This might not work if two orders end at the same time and the price of the second one is smaller:

    Tests for rent.c
    # cases
    < 5
    A single order should result in that order price
    < 1
    < 0 5 100
    > 100
    A different order price should make a different profit
    < 1
    < 0 5 110
    > 110
    With several incompatible orders profit should equal the best price
    < 3
    < 0 5 100
    < 3 7 120
    < 4 3  90
    > 120
    Given ordered orders in a small range on distinct times, profit should be optimal
    < 4
    < 0 5 100
    < 3 7 140
    < 5 9 80
    < 6 9 70
    > 180
    Profit should not decrease if a lower price is given after a greater one for the same time
    < 2
    < 0 5 170
    < 3 2 40
    > 170

This last test fails:

    diff expected.dat result.dat
    5c5
    < 170
    ---
    > 40
    make: *** [test] Error 1
    shell returned 2

To make it pass, we update the plan only if the new value is greater:

    #define MAXTIME 20
    int Plan[MAXTIME];

    int main() {
        int max_cases = get_int(Line);
        for(int c = 0; c < max_cases; c++) {
            for(int i = 0; i < MAXTIME; i++)
                Plan[i] = 0;
            int max_orders = get_int(Line);
            int profit = 0;
            for(int o = 0; o < max_orders; o++) {
                int start_time; int duration; int price;
                sscanf(get_line(Line), "%d %d %d", &start_time, &duration, &price); 
                if(Plan[start_time] > profit)
                    profit = Plan[start_time];
                if(profit + price > Plan[start_time + duration])
                    Plan[start_time + duration] = profit + price;
            }
            for(int t = 0; t < MAXTIME; t++) 
                if(Plan[t] > profit)
                    profit = Plan[t];
            printf("%d\n", profit);
        }
        return 0;
    }

Let's refactor our program a bit:

    #define MAXTIME 20
    int Plan[MAXTIME];

    void initialize() {
        for(int i = 0; i < MAXTIME; i++)
            Plan[i] = 0;
    }

    int max(int a, int b) {
        return a > b ? a : b;
    }

    int calc_profit() {
        int profit = 0;
        for(int t = 0; t < MAXTIME; t++) 
            if(Plan[t] > profit)
                profit = Plan[t];
        return profit;
    }

    int main() {
        int max_cases = get_int(Line);
        for(int c = 0; c < max_cases; c++) {
            initialize();

            int max_orders = get_int(Line);
            int profit = 0;
            for(int o = 0; o < max_orders; o++) {
                int start_time; int duration; int price;
                sscanf(get_line(Line), "%d %d %d", &start_time, &duration, &price); 
                profit = max(profit, Plan[start_time]);
                Plan[start_time + duration] = max(Plan[start_time + duration], profit + price); 
            }
            printf("%d\n", calc_profit());
        }
        return 0;
    }

Reading unsorted sequence of orders
-----------------------------------
Our program won't work if we get orders in an different sequence than ordered by start time. Let's write a test:

    Given unordered orders in a small range, profit should be optimal
    < 4
    < 6 9 70
    < 5 9 85
    < 0 5 100
    < 3 7 140
    > 185

Of course it fails:

    diff expected.dat result.dat
    6c6
    < 185
    ---
    > 140
    make: *** [test] Error 1
    shell returned 2

The way to remedy this is to sort the orders by start time before planning the rentals. We can do this with the standard library `qsort` function. This requires than we *store* the orders in an array, instead of processing them as we read them on the input stream.

    #define MAXORDER 10000
    struct order{
        int start_time;
        int duration;
        int price;
    } Orders[MAXORDER];

    int MaxOrder;

    int compare_orders(const void *a, const void *b) {
        struct order *pa = (struct order *)a;
        struct order *pb = (struct order *)b;
        return (pa->start_time - pb->start_time);
    }

    void get_orders() {
        for(int o = 0; o < MaxOrder; o++) {
            int start_time; int duration; int price;
            sscanf(get_line(Line), "%d %d %d", 
                &Orders[o].start_time, 
                &Orders[o].duration, 
                &Orders[o].price); 
        }
    }

    int main() {
        int max_cases = get_int(Line);
        for(int c = 0; c < max_cases; c++) {
            initialize();
            MaxOrder = get_int(Line);
            get_orders();
            qsort(Orders, MaxOrders, sizeof(struct order), compare_orders);
            int profit = 0;
            for(int o = 0; o < MaxOrder; o++) {
                int start_time = Orders[o].start_time;
                int end_time   = Orders[o].start_time + Orders[o].duration;
                int price      = Orders[o].price;
                profit = max(profit, Plan[start_time]);
                Plan[end_time] = max(Plan[end_time], profit + price); 
            }
            printf("%d\n", calc_profit());
        }
        return 0;
    }

All the tests pass. Let's refactor the program a bit to clarify the articulation of its logic in different parts:

    #include <stdio.h>
    #include <stdlib.h>
    #define MAXLINE 4096
    #define MAXTIME 20
    #define MAXORDER 10000

    int Plan[MAXTIME];

    struct order{
        int start_time;
        int duration;
        int price;
    } Orders[MAXORDER];

    int MaxOrder;

    char Line[MAXLINE];

    char *get_line(char *line) {
        fgets(line, MAXLINE, stdin);
        return line;
    }

    int get_int(char *line) {
        int result;
        sscanf(get_line(line), "%d", &result);
        return result;
    }

    int max(int a, int b) {
        return a > b ? a : b;
    }

    int compare_Orders(const void *a, const void *b) {
        struct order *pa = (struct order *)a;
        struct order *pb = (struct order *)b;
        return (pa->start_time - pb->start_time);
    }

    void get_Orders() {
        for(int o = 0; o < MaxOrder; o++) {
            int start_time; int duration; int price;
            sscanf(get_line(Line), "%d %d %d", 
                &Orders[o].start_time, 
                &Orders[o].duration, 
                &Orders[o].price); 
        }
    }

    void initialize() {
        for(int i = 0; i < MAXTIME; i++)
            Plan[i] = 0;
        qsort(Orders, MaxOrder, sizeof(struct order), compare_Orders);
    }

    void plan() {
        int profit = 0;
        for(int o = 0; o < MaxOrder; o++) {
            int start_time = Orders[o].start_time;
            int end_time   = Orders[o].start_time + Orders[o].duration;
            int price      = Orders[o].price;
            profit = max(profit, Plan[start_time]);
            Plan[end_time] = max(Plan[end_time], profit + price); 
        }
    }

    int calc_profit() {
        int profit = 0;
        for(int t = 0; t < MAXTIME; t++) 
            if(Plan[t] > profit)
                profit = Plan[t];
        return profit;
    }

    int main() {
        int max_cases = get_int(Line);
        for(int c = 0; c < max_cases; c++) {
            MaxOrder = get_int(Line);
            get_Orders();
            initialize();
            plan();
            printf("%d\n", calc_profit());
        }
        return 0;
    }
