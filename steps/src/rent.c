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
        int max_orders = get_int(); 
        int total = 0;
        for(int j=0; j<max_orders; j++) {
            fgets(Line, MAXLINE, stdin);
            sscanf(Line, "%d %d %d", 
                   &start_time, &duration, &price);
            total += price;
        }   
        printf("%d\n", total);
    }   
    return 0;
}
