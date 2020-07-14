#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define MAXLINE 4096
#define MAXORDER 10000
#define MAXTIMEPOINT 20000
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

void schedule_events(int start, int duration, int value) {
   Events[Max_event].start_time = start;
   Events[Max_event].end_time   = start;
   Events[Max_event].value      = 0;
   Max_event++;
   Events[Max_event].start_time = start;
   Events[Max_event].end_time   = start + duration;
   Events[Max_event].value      = value;
   Max_event++;
   Events[Max_event].start_time = start + duration;
   Events[Max_event].end_time   = start + duration;
   Events[Max_event].value      = 0;
   Max_event++;
}
int get_orders() {
    int max_orders = get_int(Line);
    Max_event = 0;
    for(int j=0; j<max_orders; j++) {
        int s,d,v;
        get_line(Line);
        sscanf(Line, "%d %d %d", &s, &d, &v);
        schedule_events(s, d, v);
    }
    return max_orders;
}

int compare_events(const void *a, const void *b) {
    struct event *pa = (struct event *)a;
    struct event *pb = (struct event *)b;
    if (pa->start_time == pb->start_time)
        return (pa->end_time - pb->end_time);
    else
        return  (pa->start_time - pb->start_time);
}

void deduplicate_events() {
    static struct event temp[MAXEVENT];
    struct event current;
    current.start_time = -1;
    current.end_time   = -1;
    int count = 0;
    for(int i=0; i < Max_event; i++) {
        if (compare_events(&Events[i], &current)) {
            temp[count] = Events[i];
            count++;
        }
        current = Events[i];
    }
    for(int i=0; i<count; i++) 
        Events[i] = temp[i];
    Max_event = count;
}

void sort_events() {
    qsort(Events, Max_event, sizeof(struct event), compare_events);
    deduplicate_events();
}

int find_event(struct event target) {
    int l = 0;
    int h = Max_event-1;
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

int calc_value() {
    int value = 0;
    for (int i=0; i < Max_event; i++) {
        struct event target;
        if (Events[i].start_time == Events[i].end_time) 
            value = max(value, Events[i].value);
        if (Events[i].start_time != Events[i].end_time) {
            target.start_time = Events[i].end_time;
            target.end_time   = target.start_time;
            int j = find_event(target);
            Events[j].value = max(Events[j].value, value + Events[i].value);
        }
    }
    return value;
}

int main() {
    int max_cases = get_int(Line);
    for(int i=0; i < max_cases; i++) {
            int max_orders = get_orders();
            sort_events();
            printf("%d\n", calc_value());
    }
}
