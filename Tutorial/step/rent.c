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

        /* read the unique order in the case */
        fgets(Line, MAXLINE, stdin);

        int start_time, duration, value;
        sscanf(Line, "%d %d %d", &start_time, &duration, &value);
        printf("%d\n", value);
    }
    return 0;
}
