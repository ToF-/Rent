#include <stdio.h>
#include <stdlib.h>

#define MAXLINE 40
#define MAXORDER 10000
#define MAXEVENT 30000

char Line[MAXLINE];

char *get_line(char *line) {
    fgets(line, MAXLINE, stdin);
    return line;
}

struct event {
    int start_time;
    int end_time;
    int value;
} Events[MAXEVENT];

int Max_event;

int max(int a, int b) {
        return a > b ? a : b ;
}

int get_int(char *line) {
    int result;
    sscanf(get_line(line),"%d", &result);
    return result;
}

void add_check_event(int start_time) {
   Events[Max_event].start_time = start_time;
   Events[Max_event].end_time   = start_time;
   Events[Max_event].value      = 0;
   Max_event++;
}

void add_schedule_event(int start_time, int duration, int value) {
   Events[Max_event].start_time = start_time;
   Events[Max_event].end_time   = start_time + duration;
   Events[Max_event].value      = value;
   Max_event++;
}
void schedule_events(int start_time, int duration, int value) {
    add_check_event(start_time);
    add_schedule_event(start_time, duration, value);
    add_check_event(start_time + duration);
}
void get_orders() {
    Max_event = 0;
    int max_order = get_int(Line);
    for(int j = 0; j < max_order; j++) {
        int start_time, duration, value;
        get_line(Line);
        sscanf(Line, "%d %d %d", &start_time, &duration, &value);
        schedule_events(start_time, duration, value);
    }
}

int compare_events(const void *a, const void *b) {
    struct event *pa = (struct event *)a;
    struct event *pb = (struct event *)b;
    if (pa->start_time == pb->start_time)
        return (pa->end_time - pb->end_time);
    else
        return  (pa->start_time - pb->start_time);
}

void sort_events() {
    qsort(Events, Max_event, sizeof(struct event), compare_events);
}

int find_event(struct event target) {
    int l = 0;
    int h = Max_event - 1;
    int m;
    while (l <= h) {
        m = l + (h - l) / 2;
        int result = compare_events(&Events[m], &target);
        if (result < 0)
            l = m + 1;
        else if (result > 0)
            h = m - 1;
        else
            return m;
    }
    return -1;
}

int is_check(struct event event) {
    return event.start_time == event.end_time;
}

int calc_value() {
    int value = 0;
    for (int i=0; i < Max_event; i++) {
        if (is_check(Events[i]))
            value = max(value, Events[i].value);
        else {
            struct event target;
            target.start_time = Events[i].end_time;
            target.end_time   = target.start_time;
            int t = find_event(target);
            Events[t].value = max(Events[t].value, value + Events[i].value);
        }
    }
    return value;
}

int main() {
    int max_cases = get_int(Line);
    for(int i=0; i < max_cases; i++) {
            get_orders();
            sort_events();
            printf("%d\n", calc_value());
    }
}
