// rent.cpp
//#define SPOJ // uncomment this line to make spoj
#include <iostream>
#include "rent.h"
#include "assert.h"
#include "stdio.h"
#include <cstdlib>

using namespace std;

Scheduler::Scheduler() {
    max_orders = 0;
}

Order Scheduler::next_compatible_order(int k, Order order) {
    int l = k+1;
    int h = max_orders;
    int end_time = order.start_time + order.duration;
    int result;
    while(l <= h) {
        int m = l + (h - l) / 2;
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
        Order compatible = next_compatible_order(i, current);
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

Order OrderReader::read() {
    Order order;
    input >> order.start_time >> order.duration >> order.amount ;
    return order;
}

void RevenueWriter::write(int revenue) {
    output << revenue << endl;

}

Session::Session(istream &input, ostream &output) : input(input), output(output) {
} 
void Session::process() {
    int max_cases, max_orders;
    OrderReader reader = OrderReader(input);
    RevenueWriter writer = RevenueWriter(output);

    input >> max_cases;
    for(int k=0; k < max_cases; k++) {
        Scheduler scheduler;
        input >> max_orders;
        for(int i=0; i<max_orders; i++) {
            Order order = reader.read();
            scheduler.add_order(order.start_time, order.duration, order.amount);
        }
        writer.write(scheduler.get_revenue());
    }
}
