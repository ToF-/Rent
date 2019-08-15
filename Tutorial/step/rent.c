#include <stdio.h>
#define MAXLINE 80
#define MAXORDER 10000

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
    return max_orders;
}

int next_compatible(int i, int max_orders) {
    int end_time = Orders[i].start_time + Orders[i].duration;
    for(int j=i+1; j<max_orders; j++)
        if(Orders[j].start_time >= end_time)
            return j;
    return max_orders;
}

int max(int a, int b) {
    return a > b ? a : b;
}
int value(int max_orders) {
    if(max_orders == 1) 
        return Orders[0].value;
    int total = 0;
    for(int i=max_orders-2; i>=0; i--) {
        int k = next_compatible(i, max_orders);
        int value_compatible = Orders[i].value +
            (k < max_orders ? Orders[k].value : 0);
        int value_next = Orders[i+1].value;
        Orders[i].value = max(value_compatible, value_next);
    }
    return Orders[0].value;
}

int main() {
    int max_cases = get_int();

    for(int i=0; i<max_cases; i++) {
        int max_orders = get_orders();
        printf("%d\n", value(max_orders));
    }
    return 0;
}
