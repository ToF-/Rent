#include <stdio.h>
#include <assert.h>
#define MAXLINE 4096
#define MAXORDER 10001

struct order{
    int start_time;
    int duration;
    int value;
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

int next_compatible(int j, int max_orders) {
    for ( int k=j+1; k<=max_orders; k++) {
        if (Orders[k].start_time >= Orders[j].start_time + Orders[j].duration) 
            return k;
    }
    return max_orders;
}

int calc_value(int max_orders) {
    for(int j=max_orders-2; j>=0; j--) {
        int k = next_compatible(j,max_orders);
        Orders[j].value = max(Orders[j+1].value, Orders[j].value + Orders[k].value);
    } 
    return Orders[0].value;
}

int get_orders() {
    int max_orders = get_int(Line);
    for(int j=0; j<max_orders; j++) {
        get_line(Line);
        sscanf(Line, "%d %d %d", 
                &Orders[j].start_time, 
                &Orders[j].duration, 
                &Orders[j].value); 
    }
    return max_orders;
}

void initialize() {
    for (int i=0; i<MAXORDER; i++) {
        Orders[i].start_time = 10000000;
        Orders[i].duration   = 0;
        Orders[i].value      = 0;
    }
}
int main() {
    int max_cases = get_int(Line);
    for(int i=0; i<max_cases; i++) {
        initialize();
        int max_orders = get_orders();
        printf("%d\n", calc_value(max_orders+1));
    }
    return 0;
}


