#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#define MAXLINE 4096
#define MAXORDER 10001

struct order{
    int start_time;
    int end_time;
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

int next_compatible_slow(int j, int max_orders) {
    int end_time = Orders[j].end_time;
    for (int k=j+1; k<=max_orders; k++)
        if(Orders[k].start_time >= end_time)
            return k;
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
        int s,d,v;
        get_line(Line);
        sscanf(Line, "%d %d %d", &s, &d, &v);

        Orders[j].start_time = s;
        Orders[j].end_time = s+d;
        Orders[j].value = v; 
    }
    return max_orders;
}

void add_sentinel(int max_orders) {
    Orders[max_orders-1].start_time = 2000000;
    Orders[max_orders-1].end_time   = 2000000;
    Orders[max_orders-1].value      = 0;
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
void sort_orders(int max_orders) {
    qsort(Orders, max_orders, sizeof(struct order), compare_orders);
}
int main() {
    int max_cases = get_int(Line);
    for(int i=0; i<max_cases; i++) {
        int max_orders = get_orders() + 1;
        add_sentinel(max_orders);
        sort_orders(max_orders);
        printf("%d\n", calc_value(max_orders));
    }
    return 0;
}


