#include <stdio.h>
#include <stdlib.h>

#define MAXLINE 80
#define MAXORDERS 10000

struct rent {
    int time;
    int duration;
    int price;
} Rent[MAXORDERS];

int maxRent;

struct cash {
    int time;
    int value;
} Cash[MAXORDERS];

int maxcashs;

int temp[MAXORDERS];

char line[MAXLINE];

void read_orders() {
    for(int i=0;i<MAXORDERS && fgets(line, MAXLINE, stdin);i++, maxRent++)
        sscanf(line, "%d %d %d", &Rent[i].time, &Rent[i].duration, &Rent[i].price);
}

int cmp_int(const void *a, const void *b) {
    int *ia = (int *)a;
    int *ib = (int *)b;
    return (*ia - *ib);
}

void define_Cash() {
    for(int i=0; i<maxRent; i++)
        temp[i] = Rent[i].time + Rent[i].duration;
    qsort(temp, maxRent, sizeof(int), cmp_int);
    int t = -1;
    int p = 0;
    for(int i=0; i<maxRent; i++) 
        if(temp[i] != t) { 
            t = temp[i];
            Cash[p].time = t;
            Cash[p].value = 0;
            p++;
        }
    maxcashs = p;
}


int cmp_rent(const void *a, const void *b) {
    struct rent *ca = (struct rent *)a;
    struct rent *cb = (struct rent *)b;
    return (ca->time - cb->time);
}

void sort_Rent() {
    qsort(Rent, maxRent, sizeof(struct rent), cmp_rent);
}

struct cash *find(int t) {
    int l = 0;
    int u = maxcashs -1;
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
    int o = 0,p = 0;

    while(p < maxcashs) {
        if (o < maxRent) {
            if (Cash[p].time <= Rent[o].time) {
                profit = cash(&Cash[p], profit);
                p++;
            }
            else {
                profit = cash(find(Rent[o].time), profit);
                rent(Rent[o], profit);
                o++;
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
    read_orders();
    define_Cash();
    sort_Rent();
    printf("%d\n", calc_profit());
}

