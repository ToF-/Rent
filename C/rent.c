#include <stdio.h>
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
    int max_cases = get_int(Line);
    for(int c = 0; c < max_cases; c++) {
        int max_orders = get_int(Line);
        int profit = 0;
        for(int o = 0; o < max_orders; o++) {
            int start_time;
            int duration;
            int price;
            sscanf(get_line(Line), "%d %d %d", &start_time, &duration, &price); 
            if(price > profit)
                profit = price;
        }
        printf("%d\n", profit);
    }
    return 0;
}
