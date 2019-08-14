#include <stdio.h>
#include <assert.h>

#define MAXLINE 80 /* arbitrary length of input line */

char Line[MAXLINE];

int get_int() {
    int result;
    fgets(Line, MAXLINE, stdin);
    sscanf(Line, "%d", &result);
    return result;
}

int main() {
    int max_cases = get_int();

    for(int i=0; i<max_cases; i++) {
        int start_time, duration, price;
        int max_orders = get_int(); /* we don't use the # of orders for now */
        assert(max_orders==1);
        fgets(Line, MAXLINE, stdin);
        sscanf(Line, "%d %d %d", &start_time, &duration, &price);
        printf("%d\n", price);
    }   
    return 0;
}
