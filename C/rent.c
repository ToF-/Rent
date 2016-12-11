#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define MAXLINE 80
#define MAXORDERS 10000

struct rent {
    int start;
    int end;
    int price;
} Rent[MAXORDERS];

int MaxRent;

struct cash {
    int time;
    int value;
} Cash[MAXORDERS*2];

int MaxCash;


char line[MAXLINE];

int read_int() {
    int i;
    fgets(line, MAXLINE, stdin);
    sscanf(line, "%d", &i);
    return i;
}

int read_orders() {
    int max = read_int();
    for (int r=0; r < max; r++) {
        int start_time;
        int duration;
        int price;
        fgets(line, MAXLINE, stdin);
        sscanf(line, "%d %d %d", &start_time, &duration, &price);
        Rent[r].start = start_time;
        Rent[r].end   = start_time + duration;
        Rent[r].price = price;
    }
    return max;
}

int compare_int(const void *a, const void *b) {
    int *ia = (int *)a;
    int *ib = (int *)b;

    return (*ia - *ib);
}

int compare_rent(const void *a, const void *b) {
    struct rent *ca = (struct rent *)a;
    struct rent *cb = (struct rent *)b;

    return (ca->start - cb->start);
}

int init_plan() {
    int temp[MAXORDERS*2];
    qsort(Rent, MaxRent, sizeof(struct rent), compare_rent);

    for(int i=0; i<MaxRent; i++) {
        temp[i*2]   = Rent[i].start;
        temp[i*2+1] = Rent[i].end;
    }
    qsort(temp, MaxRent*2, sizeof(int), compare_int);
    int t = -1;
    int p = 0;
    for(int i=0; i<MaxRent*2; i++) 
        if(temp[i] != t) { 
            t = temp[i];
            Cash[p].time = t;
            Cash[p].value = 0;
            p++;
        }
    return p;
}

struct cash *find(int start_pos, int t) {
    assert(start_pos<MaxCash);
    int l = start_pos;
    int u = MaxCash-1;
    int m = -1;

    while(l <= u) {
        m = l + (u - l) / 2;    
        if(Cash[m].time == t) 
            return (Cash+m);
        else {
            if(Cash[m].time < t) 
                l = m + 1;
            else 
                u = m -1;
        }               
    }
    return NULL;
}

int max(int a, int b) {
    return a > b ? a : b;
}

int calc_profit() {
    int profit = 0;
    for(int r=0,p=0; p<MaxCash;) {
        profit = max(profit, Cash[p].value);
        if (r < MaxRent && (Rent[r].start <= Cash[p].time)) {
            struct cash *c = find(p+1, Rent[r].end);
            assert(c != NULL);
            c->value = max(c->value, profit + Rent[r].price);
            r++;
        }
        else {
            profit = max(profit, Cash[p].value);
            p++;
        }
    }
    return profit;
}

int main() {
    int cases = read_int();
    for(int i=0; i < cases; i++) {
        MaxRent = read_orders();
        MaxCash = init_plan();
        printf("%d\n", calc_profit());
    }
}

