#include <stdio.h>
#include <stdlib.h>

#define MAXLINE 40
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

int Max_order;

int Stack[MAXSTACK];
int StackPointer = 0;

int pop() {
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

void get_orders() {
    int max_order = get_int(Line);
    for(int j = 0; j < max_order; j++) {
        int start_time, duration, price;
        get_line(Line);
        sscanf(Line, "%d %d %d", &start_time, &duration, &price);

        Orders[j].start_time = start_time;
        Orders[j].duration   = duration;
        Orders[j].price      = price;
        Orders[j].value      = NIL;
    }
    Max_order = max_order;
}

void add_sentinel() {
    Orders[Max_order].start_time = 2000000;
    Orders[Max_order].duration   = 0;
    Orders[Max_order].price      = 0;
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
        return (pa->duration - pb->duration);
}
void sort_orders() {
    qsort(Orders, Max_order, sizeof(struct order), compare_orders);
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
int calc_value() {
    push(0);
    push(NIL);
    while( StackPointer>0 ) {
        int n = pop();
        int i = pop();
        int j = (i+1) % Max_order;
        if (n == NIL) {
            n = next_compatible(i, Max_order);
            push(i);
            push(n);

            if (j < Max_order-1 && Orders[j].value == NIL) {
                push(j);
                push(NIL);
            }
            if (n < Max_order-1 && Orders[n].value == NIL) {
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
        get_orders();
        add_sentinel();
        sort_orders();
        StackPointer = 0;
         
        printf("%d\n", calc_value());
    }
    return 0;
}


