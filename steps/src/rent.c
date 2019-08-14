#include <stdio.h>
#include <assert.h>

#define MAXLINE 80 /* arbitrary length of input line */
#define MAXORDERS 10000

char Line[MAXLINE];

struct order {
    int start_time;
    int duration;
    int price;
} Orders[MAXORDERS];

int get_int() {
    int result;
    fgets(Line, MAXLINE, stdin);
    sscanf(Line, "%d", &result);
    return result;
}

void get_order(struct order *order) {
    int start_time, duration, price;
    fgets(Line, MAXLINE, stdin);
    sscanf(Line, "%d %d %d", 
            &order->start_time, 
            &order->duration, 
            &order->price);
}

int get_orders() {
    int max_orders = get_int(); 
    for(int i=0; i<max_orders; i++) {
        get_order(&Orders[i]);
    }   
    return max_orders;
}

int max(int a, int b) {
    return a > b ? a : b;
}

int profit() {
    int max_orders = get_orders();
    if (max_orders == 1)
        return Orders[0].price;
    if (Orders[0].start_time + Orders[0].duration 
        <= Orders[1].start_time)
        return Orders[0].price + Orders[1].price;
    return max(Orders[0].price, Orders[1].price) ;
}

int main() {
    int max_cases = get_int();

    for(int i=0; i<max_cases; i++) {
        printf("%d\n", profit());
    }   
    return 0;
}
