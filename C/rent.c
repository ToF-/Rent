#include <stdio.h>
#include <assert.h>
#define MAXLINE 4096
#define MAXORDER 10001

struct order{
    int start_time;
    int duration;
    int price;
} Orders[MAXORDER];

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

int main() {
    for (int i=0; i<MAXORDER; i++) {
        Orders[i].start_time = 10000000;
        Orders[i].duration   = 0;
        Orders[i].price      = 0;
    }
    int max_cases, max_orders;
    int start_time, duration, price;
    int value;
    max_cases = get_int(Line);
    for(int i=0; i<max_cases; i++) {
        max_orders = get_int(Line);
        value = 0;
        for(int j=0; j<max_orders; j++) {
            get_line(Line);
            sscanf(Line, "%d %d %d", 
                    &Orders[j].start_time, 
                    &Orders[j].duration, 
                    &Orders[j].price); 
        }
        max_orders++;
        for(int j=max_orders-2; j>=0; j--) {
            int k;
            for ( k=j+1; k<=max_orders; k++) {
                if (Orders[k].start_time >= Orders[j].start_time + Orders[j].duration) 
                    break;
            }
            Orders[j].price = max(Orders[j+1].price, Orders[j].price + Orders[k].price);
        } 
        printf("%d\n", Orders[0].price);
    }
    return 0;
}


