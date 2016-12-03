#include <stdio.h>
#include <stdlib.h>

#define MAXLINE 80
#define MAXORDERS 10000

struct order {
    int time;
    int duration;
    int price;
} orders[MAXORDERS];
int maxorders;

struct cell {
    int time;
    int value;
} plan[MAXORDERS];
int maxcells;

int temp[MAXORDERS];

char line[MAXLINE];

void read_orders() {
    for(maxorders=0;maxorders<MAXORDERS && fgets(line, MAXLINE, stdin);maxorders++)
        sscanf(line, "%d %d %d", &orders[maxorders].time, &orders[maxorders].duration, &orders[maxorders].price);
}

int cmp_int(const void *a, const void *b) {
    int *ia = (int *)a;
    int *ib = (int *)b;
    return (*ia - *ib);
}

void define_plan() {
    for(int i=0; i<maxorders; i++)
        temp[i] = orders[i].time + orders[i].duration;
    qsort(temp, maxorders, sizeof(int), cmp_int);
    int t = -1;
    int p = 0;
    for(int i=0; i<maxorders; i++) 
        if(temp[i] != t) { 
            t = temp[i];
            plan[p].time = t;
            plan[p].value = 0;
            p++;
        }
    maxcells = p;
}


int cmp_order(const void *a, const void *b) {
    struct order *ca = (struct order *)a;
    struct order *cb = (struct order *)b;
    return (ca->time - cb->time);
}

void sort_orders() {
    qsort(orders, maxorders, sizeof(struct order), cmp_order);
}

struct cell *find(int t) {
    int l = 0;
    int u = maxcells -1;
    int m = -1;
    int index = -1;

    while(l <= u) {
        m = l + (u - l) / 2;    
        if(plan[m].time == t) 
            return (plan+m);
        else {
            if(plan[m].time < t) 
                l = m + 1;
            else 
                u = m -1;
        }               
    }
    return NULL;
}
void rent(int time, int price) {
    struct cell *cellcash = find(time);
    if (cellcash->value < price)
        cellcash->value = price;
}


int calc_profit() {
    int profit = 0;
    int o = 0;
    int c = 0;
    while(o < maxorders || c < maxcells) {
        if(o == maxorders || plan[c].time <= orders[o].time) {
            if (plan[c].value > profit)
                profit = plan[c].value;
            c++;
        }
        else if(o < maxorders) {
            struct cell *cellcash = find(orders[o].time);
            int cash = cellcash ? cellcash->value: 0;
            if (cash > profit)
                profit = cash;
            rent(orders[o].time + orders[o].duration, profit + orders[o].price);
            o++;
        }
    }
    return profit;
}

int main() {
    read_orders();
    define_plan();
    sort_orders();
    printf("%d\n", calc_profit());
}

