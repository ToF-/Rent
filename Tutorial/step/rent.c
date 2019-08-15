#include <stdio.h>
#define MAXLINE 80

char Line[MAXLINE];

int main() {
    int max_cases;
    fgets(Line, MAXLINE, stdin);
    sscanf(Line, "%d", &max_cases);

    for(int i=0; i<max_cases; i++) {
        /* read the number of orders and ignore it */
        fgets(Line, MAXLINE, stdin); 
        int max_orders;
        sscanf(Line, "%d", &max_orders);

        int total = 0;
        for(int j=0; j<max_orders; j++) {
            int start_time, duration, value;

            fgets(Line, MAXLINE, stdin);
            sscanf(Line, "%d %d %d", &start_time, &duration, &value);
            total += value;
        }
        printf("%d\n", total);
    }
    return 0;
}
