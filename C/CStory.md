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

Testing the program
-------------------

In order to test our program, we use a test input file that will grow as our understanding of the solution grows, and `diff` the results with the expected result. Here's a Makefile:

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

Producing output values
-----------------------

Let's make the test pass real quick:

    #include <stdio.h>

    int main() {
        printf("%d\n", 100);
        return 0;
    }


Of course this is widely incomlete. The program should at least process different orders with different prices:

    2
    1
    0 5 100
    1
    3 7 140


    100
    140

the test fails:

    ./rent <test.dat >result.dat; diff expected.dat result.dat
    2d1
    < 140
    make: *** [test] Error 1

Here's a ugly, naive way to make it pass:

    #include <stdio.h>
    #define MAXLINE 4096

    char line[MAXLINE];

    int main() {
        fgets(line, MAXLINE, stdin);
        fgets(line, MAXLINE, stdin);
        int start_time;
        int duration;
        int price;
        fgets(line, MAXLINE, stdin);
        sscanf(line, "%d %d %d", &start_time, &duration, &price); 
        printf("%d\n", price);
        fgets(line, MAXLINE, stdin);
        fgets(line, MAXLINE, stdin);
        sscanf(line, "%d %d %d", &start_time, &duration, &price); 
        printf("%d\n", price);
        return 0;
    }

Let's refactor the code a bit, extracting routines:

    #include <stdio.h>
    #define MAXLINE 4096

    struct order {
        int start_time;
        int duration;
        int price;
    };

    char Line[MAXLINE];

    char *get_line() {
        return fgets(Line, MAXLINE, stdin);
    }

    int get_int() {
        int result;
        sscanf(get_line(), "%d", &result);
        return result;
    }

    void get_order(struct order *order) {
        int s,d,p;
        sscanf(get_line(), "%d %d %d", &s, &d, &p);
        order->price = p; 
    }

    int main() {
        int max = get_int();
        for(int i=0; i<max; i++) {
            get_int();
            struct order order;
            get_order(&order);
            printf("%d\n", order.price );
        }
        return 0;
    }

What about having more than 1 order per case? Let's add a test:

    3
    1
    0 5 100
    1
    3 7 140
    2
    0 5 100
    3 7 150

    100
    140
    150

Here's the code to make this pass. 

    int get_orders() {
        int max_order = get_int();
        for(int o=0; o<max_order; o++) {
            get_order(&Orders[o]);
        }
        return max_order;
    }

    int calc_profit(int max_order) {
        int profit = 0;
        for(int o=0; o<max_order; o++) {
            if(Orders[o].price>profit)
                profit = Orders[o].price;
        }
        return profit;
    }

    int main() {
        int max_case = get_int();
        for(int c=0; c<max_case; c++) {
            printf("%d\n", calc_profit(get_orders()));   
        }
        return 0;
    }

We simply take the maximum price in the list of orders. 
