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

