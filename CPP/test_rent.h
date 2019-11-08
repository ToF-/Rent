// test_rent.h

#include <cxxtest/TestSuite.h>
#include "rent.h"

class TestLags : public CxxTest::TestSuite {
public:

void testWhenNoOrderGivenRevenueIsZero(void) {
    Scheduler scheduler;
    TS_ASSERT_EQUALS(0, scheduler.get_revenue());
}

void testAfterOneOrderRevenueIsThisOrder(void) {
    Scheduler scheduler;
    scheduler.add_order(0,5,10);
    TS_ASSERT_EQUALS(10, scheduler.get_revenue());
}

void testAfteriTwoCompatibleOrdersRevenueIsTheSumofOrders(void) {
    Scheduler scheduler;
    scheduler.add_order(0,5,10);
    scheduler.add_order(5,5,8);
    TS_ASSERT_EQUALS(18, scheduler.get_revenue());
}

void testAfteriTwoIncompatibleOrdersRevenueIsTheMaxofOrders(void) {
    Scheduler scheduler;
    scheduler.add_order(0,5,10);
    scheduler.add_order(3,5,14);
    TS_ASSERT_EQUALS(14, scheduler.get_revenue());
}

void testAfterSeveralCompatibleOrIncompatibleOrdersRevenueIsTheBestofOrders(void) {
    Scheduler scheduler;
    scheduler.add_order(0,5,10);
    scheduler.add_order(3,10,14);
    scheduler.add_order(5,6,7);
    scheduler.add_order(6,6,8);
    TS_ASSERT_EQUALS(18, scheduler.get_revenue());
}

void testAfterSeveralCompatibleOrIncompatibleUnsortedOrdersRevenueIsTheBestofOrders(void) {
    Scheduler scheduler;
    scheduler.add_order(0,5,10);
    scheduler.add_order(5,6,7);
    scheduler.add_order(3,10,14);
    scheduler.add_order(6,6,8);
    TS_ASSERT_EQUALS(18, scheduler.get_revenue());
}
};
