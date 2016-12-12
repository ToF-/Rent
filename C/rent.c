#include <stdio.h>
#define MAXLINE 4096

char line[MAXLINE];

int main() {
    fgets(line, MAXLINE, stdin);
    fgets(line, MAXLINE, stdin);
    int start_time;
    int duration;
    int price;
    fgets(line, MAXLINE, stdin);
    sscanf(line, "%d %d %d", &start_time, &duration, &price); 
    printf("%d\n", price);
    fgets(line, MAXLINE, stdin);
    fgets(line, MAXLINE, stdin);
    sscanf(line, "%d %d %d", &start_time, &duration, &price); 
    printf("%d\n", price);
    return 0;
}
