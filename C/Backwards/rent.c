#include <stdio.h>
#include <stdlib.h>
#define MAXLINE 40
#define MAXORDER 10001
#define MAXTIME 2000000

struct order{
    int start_time;
    int end_time;
    int value;
} Orders[MAXORDER];

int Max_order;

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
    int l = j+1;
    int h = max_orders;
    int m;
    int end_time = Orders[j].end_time;
    int result;
    while (l <= h) {
        m = l + (h - l) / 2;
        if(Orders[m].start_time < end_time)
            l = m + 1;
        else {
            result = m;
            h = m - 1;
        }
    }
    return result;
}

int calc_value() {
    for(int j = Max_order-2; j >= 0; j--) {
        int k = next_compatible(j, Max_order);
        Orders[j].value = max(Orders[j+1].value, Orders[j].value + Orders[k].value);
    } 
    return Orders[0].value;
}

void get_orders() {
    int max_order = get_int(Line);
    for(int j = 0; j < max_order; j++) {
        int start_time, duration, value;
        get_line(Line);
        sscanf(Line, "%d %d %d", &start_time, &duration, &value);

        Orders[j].start_time = start_time;
        Orders[j].end_time   = start_time+duration;
        Orders[j].value      = value; 
    }
    Max_order = max_order;
}

void add_sentinel() {
    Orders[Max_order].start_time = MAXTIME;
    Orders[Max_order].end_time   = MAXTIME;
    Orders[Max_order].value      = 0;
    Max_order++;
}

int compare_orders(const void *a, const void *b) {
    struct order *pa = (struct order *)a;
    struct order *pb = (struct order *)b;
    if (pa->start_time < pb->start_time)
        return -1;
    else if (pa->start_time > pb->start_time)
        return 1;
    else 
        return (pa->end_time - pb->end_time);
}
void sort_orders() {
    qsort(Orders, Max_order, sizeof(struct order), compare_orders);
}
int main() {
    int max_cases = get_int(Line);
    for(int i=0; i<max_cases; i++) {
        get_orders();
        add_sentinel();
        sort_orders();
        printf("%d\n", calc_value());
    }
    return 0;
}


