#include <stdio.h>
#include <stdlib.h>
#define MAXLINE 4096
#define MAXTIME 20
#define MAXORDER 20000

int Plan[MAXTIME];

struct order{
    int start_time;
    int duration;
    int price;
} Orders[MAXORDER];

int MaxOrder;

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

int compare_Orders(const void *a, const void *b) {
    struct order *pa = (struct order *)a;
    struct order *pb = (struct order *)b;
    return (pa->start_time - pb->start_time);
}

void get_Orders() {
    for(int o = 0; o < MaxOrder; o++) {
        int start_time; int duration; int price;
        sscanf(get_line(Line), "%d %d %d", 
            &Orders[o*2].start_time, 
            &Orders[o*2].duration, 
            &Orders[o*2].price); 
            Orders[o*2+1].start_time = Orders[o*2].start_time + Orders[o*2].duration;
            Orders[o*2+1].duration = 0;
            Orders[o*2+1].price = 0; 
    }
    MaxOrder*=2;
}

void initialize() {
    for(int i = 0; i < MAXTIME; i++)
        Plan[i] = 0;
    qsort(Orders, MaxOrder, sizeof(struct order), compare_Orders);
}

int calc_profit() {
    int profit = 0;
    for(int o = 0; o < MaxOrder; o++) {
        int start_time = Orders[o].start_time;
        int end_time   = Orders[o].start_time + Orders[o].duration;
        int price      = Orders[o].price;
        profit = max(profit, Plan[start_time]);
        Plan[end_time] = max(Plan[end_time], profit + price); 
    }
    return profit;
}

int main() {
    int max_cases = get_int(Line);
    for(int c = 0; c < max_cases; c++) {
        MaxOrder = get_int(Line);
        get_Orders();
        initialize();
        printf("%d\n", calc_profit());
    }
    return 0;
}
