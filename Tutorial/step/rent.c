#include <stdio.h>
#include <stdlib.h>
#define MAXLINE 80
#define MAXORDER 10001
#define MAXSTARTTIME 2000000

char Line[MAXLINE];

struct order {
    int start_time;
    int duration;
    int value;
} Orders[MAXORDER];

int get_int() {
    int result;
    fgets(Line, MAXLINE, stdin);
    sscanf(Line, "%d", &result);
    return result;
}

void get_order(struct order *order) {

    fgets(Line, MAXLINE, stdin);
    sscanf(Line, "%d %d %d", 
            &order->start_time, 
            &order->duration, 
            &order->value);
}

int get_orders() {
    int max_orders = get_int();
    for(int j=0; j<max_orders; j++) {
        get_order(&Orders[j]);
    }
    /* adding a sentinel to simplify search of compatible order */
    Orders[max_orders].start_time = MAXSTARTTIME;
    Orders[max_orders].duration   = 0;
    Orders[max_orders].value      = 0;

    return max_orders+1;
}

int compare_orders(const void *a, const void *b) {
    struct order *pa = (struct order *)a;
    struct order *pb = (struct order *)b;
    if (pa->start_time < pb->start_time)
        return -1;
    else if (pa->start_time > pb->start_time)
        return 1;
    else 
        return 0;
}

void sort_orders(int max_orders) {
    qsort(Orders, max_orders, sizeof(struct order), compare_orders);
}

int next_compatible(int i, int max_orders) {
    int end_time = Orders[i].start_time + Orders[i].duration;
    int low  = i+1;
    int high = max_orders;
    int middle;
    int result;
    while(low <= high) {
        middle = low + ((high - low) / 2);
        if (Orders[middle].start_time < end_time) {
            low = middle + 1;
        }
        else {
            result = middle;
            high = middle - 1;
        }
    }
    return result;
}

int max(int a, int b) {
    return a > b ? a : b;
}

int value(int max_orders) {
    int total = 0;
    for(int i=max_orders-2; i>=0; i--) {
        int k = next_compatible(i, max_orders);
        int value_compatible = Orders[i].value + Orders[k].value;
        int value_next = Orders[i+1].value;
        Orders[i].value = max(value_compatible, value_next);
    }
    return Orders[0].value;
}

int main() {
    int max_cases = get_int();

    for(int i=0; i<max_cases; i++) {
        int max_orders = get_orders();
        sort_orders(max_orders);
        printf("%d\n", value(max_orders));
    }
    return 0;
}
