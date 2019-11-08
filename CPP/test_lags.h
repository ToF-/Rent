// test_lags.h

#include <cxxtest/TestSuite.h>
#include "lags.h"

class TestLags : public CxxTest::TestSuite {
public:

void testWhenNoOrderGivenRevenueIsZero(void) {
    Scheduler scheduler;
    TS_ASSERT_EQUALS(0, scheduler.get_revenue());
}

};
