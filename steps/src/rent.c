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

int Max_orders;

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

int next_compatible(int i) {
    int end = Orders[i].start_time + Orders[i].duration;
    
    int j;
    for(j=i+1; j < Max_orders && Orders[j].start_time < end; j++);
    return j;
}

int profit() {
    Max_orders = get_orders();
    if (Max_orders == 1)
        return Orders[0].price;
    for(int i=Max_orders-2; i>=0; i--) {
        int k = next_compatible(i);
        int value_with_next_compatible =
            Orders[i].price + (k < Max_orders ? Orders[k].price : 0);

        Orders[i].price = max(value_with_next_compatible, 
                              Orders[i+1].price);
    }   
    return Orders[0].price;
}

int main() {
    int max_cases = get_int();

    for(int i=0; i<max_cases; i++) {
        printf("%d\n", profit());
    }   
    return 0;
}
