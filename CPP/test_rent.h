// test_rent.h


#include <cxxtest/TestSuite.h>
#include "rent.h"
#include <string>
#include <iostream>
#include <sstream>
#include <list>

using namespace std;

class TestLags : public CxxTest::TestSuite {
public:

void test_When_No_Order_Given_Revenue_Is_Zero(void) {
    Scheduler scheduler;
    TS_ASSERT_EQUALS(0, scheduler.get_revenue());
}

void test_after_one_order_revenue_is_this_order(void) {
    Scheduler scheduler;
    scheduler.add_order(0,5,10);
    TS_ASSERT_EQUALS(10, scheduler.get_revenue());
}

void test_after_two_compatible_orders_revenue_is_the_sumof_orders(void) {
    Scheduler scheduler;
    scheduler.add_order(0,5,10);
    scheduler.add_order(5,5,8);
    TS_ASSERT_EQUALS(18, scheduler.get_revenue());
}

void test_after_two_incompatible_orders_revenue_is_the_maxof_orders(void) {
    Scheduler scheduler;
    scheduler.add_order(0,5,10);
    scheduler.add_order(3,5,14);
    TS_ASSERT_EQUALS(14, scheduler.get_revenue());
}

void test_after_several_compatible_or_incompatible_orders_revenue_is_the_best_of_orders(void) {
    Scheduler scheduler;
    scheduler.add_order(0,5,10);
    scheduler.add_order(3,10,14);
    scheduler.add_order(5,6,7);
    scheduler.add_order(6,6,8);
    TS_ASSERT_EQUALS(18, scheduler.get_revenue());
}

void test_after_several_compatible_or_incompatible_unsorted_orders_revenue_is_the_best_of_orders(void) {
    Scheduler scheduler;
    scheduler.add_order(0,5,10);
    scheduler.add_order(5,6,7);
    scheduler.add_order(3,10,14);
    scheduler.add_order(6,6,8);
    TS_ASSERT_EQUALS(18, scheduler.get_revenue());
}

void test_Order_reader_should_read_an_order_from_a_stream() {
    istringstream stream("42 17 4807\n");
    OrderReader reader(stream);
    Order order = reader.read();
    TS_ASSERT_EQUALS(42, order.start_time);
    TS_ASSERT_EQUALS(17, order.duration);
    TS_ASSERT_EQUALS(4807, order.amount);
}

void test_revenue_write_should_write_a_revenue_into_a_stream() {
    ostringstream stream;
    RevenueWriter writer(stream);
    writer.write(42);
    TS_ASSERT_EQUALS("42\n", stream.str());
}

void test_session_should_read_a_list_of_orders_and_output_revenue() {
    istringstream istream("1\n2\n42 17 4000\n0 15 807\n");
    ostringstream ostream;
    Session session = Session(istream, ostream);
    session.process();
    TS_ASSERT_EQUALS("4807\n", ostream.str());
}
void test_session_should_read_a_list_of_cases_and_output_revenues() {
    istringstream istream("2\n2\n42 17 4000\n0 15 807\n1\n0 17 100\n");
    ostringstream ostream;
    Session session = Session(istream, ostream);
    session.process();
    TS_ASSERT_EQUALS("4807\n100\n", ostream.str());
}


};
