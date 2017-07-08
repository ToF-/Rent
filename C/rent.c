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

int main() {
    int max_cases, max_orders;
    int start_time, duration, price;
    max_cases = get_int(Line);
    for(int i=0; i<max_cases; i++) {
        get_line(Line);
        get_line(Line);
        sscanf(Line, "%d %d %d", &start_time, &duration, &price); 
        printf("%d\n", price);
    }
    return 0;
}


