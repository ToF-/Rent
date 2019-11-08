// test_rent.h

#include <cxxtest/TestSuite.h>
#include "rent.h"

class TestLags : public CxxTest::TestSuite {
public:

void testWhenNoOrderGivenRevenueIsZero(void) {
    Scheduler scheduler;
    TS_ASSERT_EQUALS(0, scheduler.get_revenue());
}

};
