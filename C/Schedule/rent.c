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

struct order{
    int start_time;
    int duration;
    int price;
}Orders[MAXORDER];

int Time_points[MAXTIMEPOINT];
int Value[MAXTIMEPOINT];

struct event {
    int start_time;
    int type;
    int end_time;
    int price;
} Events[MAXEVENT];

int max(int a, int b) {
        return a > b ? a : b ;
}

int get_int(char *line) {
    int result;
    sscanf(get_line(line),"%d", &result);
    return result;
}
int get_orders() {
    int max_orders = get_int(Line);
    for(int j=0; j<max_orders; j++) {
        int s,d,v;
        get_line(Line);
        sscanf(Line, "%d %d %d", &s, &d, &v);

        Orders[j].start_time = s;
        Orders[j].duration = d;
        Orders[j].price = v; 
    }
    return max_orders;
}

int compare_time_points(const void *a, const void *b) {
    int *pa = (int *)a;
    int *pb = (int *)b;
    if (*pa < *pb)
        return -1;
    else if (*pa > *pb)
        return 1;
    else 
        return 0;
}
void sort_time_points(int max_time_points) {
    qsort(Time_points, max_time_points, sizeof(int), compare_time_points);
}

int collect_time_points(int max_orders) {
    assert(max_orders>0);
    int max_point = 0;
    for(int i=0; i<max_orders; i++) {
        Time_points[max_point++] = Orders[i].start_time;
        Time_points[max_point++] = Orders[i].start_time + Orders[i].duration;
        }
    sort_time_points(max_point);
    assert(max_point>0);    
    return max_point;
}

int find_time_point(int time_point, int max_time_points) {
    assert(time_point >= 0);
    assert(max_time_points>0);
    int l = 0;
    int h = max_time_points-1;
    int m;
    while (l <= h) {
        m = l + (h - l) / 2;
        if (Time_points[m] < time_point) 
            l = m + 1;
        else if (Time_points[m] > time_point)
            h = m - 1;
        else 
            return m;
    }
    return -1;
}

int schedule_events(int max_orders, int max_time_points) {
    int max_event = 0;
    for(int i=0; i<max_orders; i++) {
        int start = Orders[i].start_time;
        int end   = Orders[i].start_time + Orders[i].duration;
        assert(end > 0);
        Events[max_event].start_time = start;
        Events[max_event].end_time =   start;
        Events[max_event].price = 0;
        max_event++;
        Events[max_event].start_time = end;
        Events[max_event].end_time =   end;
        Events[max_event].price = 0;
        max_event++;
        Events[max_event].start_time = start;
        Events[max_event].end_time = end;
        Events[max_event].price = Orders[i].price;
        max_event++;
    }
    return max_event;
}

int compare_events(const void *a, const void *b) {
    struct event *pa = (struct event *)a;
    struct event *pb = (struct event *)b;
    if (pa->start_time < pb->start_time)
        return -1;
    else if (pa->start_time > pb->start_time)
        return 1;
    else  
        return (pa->type - pb->type);
}
void sort_events(int max_event) {
    qsort(Events, max_event, sizeof(struct event), compare_events);
}

int calc_value(int max_events, int max_time_points) {
    for(int i=0; i<max_time_points; i++)
        Value[i] = 0;
    int value = 0;
    for (int i=0; i < max_events; i++) {
        int start = find_time_point(Events[i].start_time, max_time_points);
        Value[start] = max(Value[start],value);
        value = max(Value[start], value);
        int end   = find_time_point(Events[i].end_time, max_time_points);
        int price = Events[i].price;
        Value[end] = max(Value[end], value + price);
    }
    return value;
}

int main() {
    int max_cases = get_int(Line);
    for(int i=0; i < max_cases; i++) {
            int max_orders = get_orders();
            assert(max_orders>0);
            int max_time_points = collect_time_points(max_orders);
            assert(max_time_points>0);
            int max_events = schedule_events(max_orders, max_time_points);
            sort_events(max_events);
            printf("%d\n", calc_value(max_events, max_time_points));
    }
}
