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

int main() {
    int max_cases = get_int();

    for(int i=0; i<max_cases; i++) {
        int max_orders = get_int();

        int total = 0;
        for(int j=0; j<max_orders; j++) {
            get_order(&Orders[j]);
        }
        if(max_orders == 1) {
            total = Orders[0].value;
        } else {
            if (Orders[0].start_time + Orders[0].duration
                > Orders[1].start_time) {

                if (Orders[0].value > Orders[1].value) 
                    total = Orders[0].value;
                else
                    total = Orders[1].value;
            }
            else {
                total = Orders[0].value + Orders[1].value;
            }
        }
        printf("%d\n", total);
    }
    return 0;
}
