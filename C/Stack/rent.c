
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define MAXLINE 4096
#define MAXORDER 10001
#define MAXSTACK 60001
#define NIL (-1)





char Line[MAXLINE];

char *get_line(char *line) {
    fgets(line, MAXLINE, stdin);
    return line;
}

struct order {
    int start_time;
    int duration;
    int price;
    int value;
}Orders[MAXORDER];

int Stack[MAXSTACK];
int StackPointer = 0;
int pop() {
    assert(StackPointer >= 0);
    StackPointer--;
    return Stack[StackPointer];
}

void push(int x) {
    Stack[StackPointer] = x;
    StackPointer++;
}

int max(int a, int b) {
        return a > b ? a : b ;
}

int get_int(char *line) {
    int result;
    sscanf(get_line(line),"%d", &result);
    return result;
}

int get_orders() {
    int max_orders = get_int(Line);
    for(int j=0; j<max_orders; j++) {
        int s,d,p;
        get_line(Line);
        sscanf(Line, "%d %d %d", &s, &d, &p);

        Orders[j].start_time = s;
        Orders[j].duration = d;
        Orders[j].price = p;
        Orders[j].value = NIL;
    }
    return max_orders;
}

void add_sentinel(int max_orders) {
    Orders[max_orders-1].start_time = 2000000;
    Orders[max_orders-1].duration   = 0;
    Orders[max_orders-1].price      = 0;
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
        return (pa->duration - pb->duration);
}
void sort_orders(int max_orders) {
    qsort(Orders, max_orders, sizeof(struct order), compare_orders);
}

int next_compatible(int j, int max_orders) {
    int l = j+1;
    int h = max_orders;
    int m;
    int end_time = Orders[j].start_time + Orders[j].duration;
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
int calc_value(int max_orders) {
    push(0);
    push(NIL);
    while( StackPointer>0 ) {
        int n = pop();
        int i = pop();
        int j = (i+1) % max_orders;
        if (n == NIL) {
            n = next_compatible(i, max_orders);
            push(i);
            push(n);

            if (j < max_orders-1 && Orders[j].value == NIL) {
                push(j);
                push(NIL);
            }
            if (n < max_orders-1 && Orders[n].value == NIL) {
                push(n);
                push(NIL);
            }
        }
        else {
            Orders[i].value = max(
                    Orders[i].price + Orders[n].value,
                    Orders[j].value);
        }

    }
    return Orders[0].value;
}

int main() {
    int max_cases = get_int(Line);
    for(int i=0; i<max_cases; i++) {
        int max_orders = get_orders() + 1;
        add_sentinel(max_orders);
        sort_orders(max_orders);
        StackPointer = 0;
         
        printf("%d\n", calc_value(max_orders));
    }
    return 0;
}


