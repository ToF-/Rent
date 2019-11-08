// rent.cpp
#include "rent.h"
#include "assert.h"
#include "stdio.h"

#undef TEST 
Scheduler::Scheduler() {
    max_orders = 0;
}

Order Scheduler::next_compatible_order(int k,int end_time) {
    int l = k+1;
    int h = max_orders;
    int m;
    int result = -1;
    while(l <= h) {
        m = l + (h - l) / 2;
        if(orders[m].start_time < end_time)
            l = m + 1;
        else {
            result = m;
            h = m - 1;
        }
    }
    return orders[result];
}

int compareOrders (const void * a, const void * b)
{
    Order orderA = *(Order *)a;
    Order orderB = *(Order *)b;
    return orderA.start_time - orderB.start_time;
}

int Scheduler::get_revenue() {
    
    orders[max_orders].start_time = MAX_START_TIME;
    orders[max_orders].duration  = 0;
    orders[max_orders].amount    = 0;
    qsort (orders, max_orders, sizeof(Order), compareOrders);
    for(int i = max_orders-1; i>=0; i--) {
        Order &current    = orders[i];
        Order next       = orders[i+1];
        Order compatible = next_compatible_order(i,current.start_time + current.duration);
        int comp_amount = current.amount + compatible.amount;
        current.amount = next.amount > comp_amount ? next.amount : comp_amount;
    }
    return orders[0].amount;
}

void Scheduler::add_order(int start, int duration, int amount) {
    assert(max_orders<MAX_ORDERS-1);
    orders[max_orders].start_time = start;
    orders[max_orders].duration   = duration;
    orders[max_orders].amount     = amount;
    max_orders++;
}
#ifndef TEST
#include <iostream>
using namespace std;

int main() {
    int max_cases;
    cin >> max_cases;
    for(int i=0; i<max_cases; i++) {
        int max_orders;
        cin >> max_orders;
        Scheduler scheduler;
        for(int j=0; j<max_orders; j++) {
            int start_time, duration, amount;
            cin >> start_time >> duration >> amount;
            scheduler.add_order(start_time, duration, amount);
        }
        cout << scheduler.get_revenue() << endl;
    }
}
#endif
