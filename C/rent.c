#include <stdio.h>
#include <stdlib.h>
#define MAXLINE 4096
#define MAXORDER 20000

struct cell{
    int time;
    int value;
} Plan[MAXORDER];

int MaxCell;

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
    if (pa->start_time < pb->start_time)
        return -1;
    else if (pa->start_time > pb->start_time)
        return 1;
    else 
        return (pa->duration - pb->duration);
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
    qsort(Orders, MaxOrder, sizeof(struct order), compare_Orders);
    MaxCell = 0;
    int t = -1;
    for(int i = 0; i < MaxOrder; i++) 
        if(Orders[i].start_time != t) {
            t = Orders[i].start_time;
            Plan[MaxCell].time = t;
            Plan[MaxCell].value = 0;
            MaxCell++;
        }
}

struct cell *plan(int time) {
    int l = 0;
    int h = MaxCell-1;
    int m;
    while(l <= h) {
        m = l + (h - l) / 2;
        if(Plan[m].time == time)
            return &Plan[m];
        else {
            if(Plan[m].time < time)
                l = m + 1;
            else
                h = m - 1;
        }
    }
    return 0;
}

int calc_profit() {
    int profit = 0;
    for(int o = 0; o < MaxOrder; o++) {
        int start_time = Orders[o].start_time;
        int end_time   = Orders[o].start_time + Orders[o].duration;
        int price      = Orders[o].price;
        if(Orders[o].duration){
            struct cell *p = plan(end_time);
            p->value = max(p->value, profit + price);
        }
        else
            profit = max(profit, plan(start_time)->value);
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
