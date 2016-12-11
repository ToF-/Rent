#include <stdio.h>
#define MAXLINE 4096
#define MAXORDER 10000

struct order {
    int start_time;
    int duration;
    int price;
} Orders[MAXORDER];

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
