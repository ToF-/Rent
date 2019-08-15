#include <stdio.h>
#define MAXLINE 80

char Line[MAXLINE];

struct order {
    int start_time;
    int duration;
    int value;
};

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

int main() {
    int max_cases = get_int();

    for(int i=0; i<max_cases; i++) {
        int max_orders = get_int();

        int total = 0;
        for(int j=0; j<max_orders; j++) {
            struct order order;
            get_order(&order);
            total += order.value;
        }
        printf("%d\n", total);
    }
    return 0;
}
