#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define MAXLINE 80
#define MAXORDERS 10000

struct rent {
    int time;
    int duration;
    int price;
} Rent[MAXORDERS];

int MaxRent;

struct cash {
    int time;
    int value;
} Cash[MAXORDERS];

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
    for (int r=0; r < max; r ++) {
        fgets(line, MAXLINE, stdin);
        sscanf(line, "%d %d %d",
            &Rent[r].time, 
            &Rent[r].duration, 
            &Rent[r].price);
    }
    return max;
}

int cmp_int(const void *a, const void *b) {
    int *ia = (int *)a;
    int *ib = (int *)b;

    return (*ia - *ib);
}

int init_plan() {
    int temp[MAXORDERS];

    for(int i=0; i<MaxRent; i++)
        temp[i] = Rent[i].time + Rent[i].duration;
    qsort(temp, MaxRent, sizeof(int), cmp_int);
    int t = -1;
    int p = 0;
    for(int i=0; i<MaxRent; i++) 
        if(temp[i] != t) { 
            t = temp[i];
            Cash[p].time = t;
            Cash[p].value = 0;
            p++;
        }
    return p;
}

int cmp_rent(const void *a, const void *b) {
    struct rent *ca = (struct rent *)a;
    struct rent *cb = (struct rent *)b;

    return (ca->time - cb->time);
}

void sort_Rent() {
    qsort(Rent, MaxRent, sizeof(struct rent), cmp_rent);
}

struct cash *find(int t) {
    int l = 0;
    int u = MaxCash -1;
    int m = -1;
    int index = -1;

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

int cash(struct cash *p, int profit) {
    if (p != NULL && p->value > profit)
        return p-> value;
    else
       return profit;
}

void rent(struct rent rent, int profit) {
    int time = rent.time + rent.duration;
    int price = rent. price + profit;
    struct cash *cash = find(time);

    if (cash->value < price)
        cash->value = price;
}


int calc_profit() {
    int profit = 0;
    int r = 0,p = 0;

    while(p < MaxCash) {
        if (r < MaxRent) {
            if (Cash[p].time <= Rent[r].time) {
                profit = cash(&Cash[p], profit);
                p++;
            }
            else {
                profit = cash(find(Rent[r].time), profit);
                rent(Rent[r], profit);
                r++;
            }    
        }
        else {
            profit = cash(&Cash[p], profit);
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
        sort_Rent();
        printf("%d\n", calc_profit());
    }
}

