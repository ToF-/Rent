#include <stdio.h>
#include <assert.h>
#define MAXLINE 4096

char Line[MAXLINE];

char *get_line(char *line) {
    fgets(line, MAXLINE, stdin);
    return line;
}

int get_int(char *line) {
    int result;
    sscanf(get_line(line), "%d", &result);
    return result;
}

int max(int a, int b) {
    return a > b ? a : b;
}

int main() {
    int max_cases, max_orders;
    int start_time, duration, price;
    int value;
    max_cases = get_int(Line);
    for(int i=0; i<max_cases; i++) {
        max_orders = get_int(Line);
        value = 0;
        for(int j=0; j<max_orders; j++) {
            get_line(Line);
            sscanf(Line, "%d %d %d", &start_time, &duration, &price); 
            value = max(value, price);           
        }
        printf("%d\n", value);
    }
    return 0;
}


