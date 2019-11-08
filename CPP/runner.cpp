/* Generated file, do not edit */

#ifndef CXXTEST_RUNNING
#define CXXTEST_RUNNING
#endif

#define _CXXTEST_HAVE_STD
#include <cxxtest/TestListener.h>
#include <cxxtest/TestTracker.h>
#include <cxxtest/TestRunner.h>
#include <cxxtest/RealDescriptions.h>
#include <cxxtest/TestMain.h>
#include <cxxtest/ErrorPrinter.h>

int main( int argc, char *argv[] ) {
 int status;
    CxxTest::ErrorPrinter tmp;
    CxxTest::RealWorldDescription::_worldName = "cxxtest";
    status = CxxTest::Main< CxxTest::ErrorPrinter >( tmp, argc, argv );
    return status;
}
bool suite_TestLags_init = false;
#include "test_rent.h"

static TestLags suite_TestLags;

static CxxTest::List Tests_TestLags = { 0, 0 };
CxxTest::StaticSuiteDescription suiteDescription_TestLags( "test_rent.h", 6, "TestLags", suite_TestLags, Tests_TestLags );

static class TestDescription_suite_TestLags_testWhenNoOrderGivenRevenueIsZero : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_testWhenNoOrderGivenRevenueIsZero() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 9, "testWhenNoOrderGivenRevenueIsZero" ) {}
 void runTest() { suite_TestLags.testWhenNoOrderGivenRevenueIsZero(); }
} testDescription_suite_TestLags_testWhenNoOrderGivenRevenueIsZero;

static class TestDescription_suite_TestLags_testAfterOneOrderRevenueIsThisOrder : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_testAfterOneOrderRevenueIsThisOrder() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 14, "testAfterOneOrderRevenueIsThisOrder" ) {}
 void runTest() { suite_TestLags.testAfterOneOrderRevenueIsThisOrder(); }
} testDescription_suite_TestLags_testAfterOneOrderRevenueIsThisOrder;

static class TestDescription_suite_TestLags_testAfteriTwoCompatibleOrdersRevenueIsTheSumofOrders : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_testAfteriTwoCompatibleOrdersRevenueIsTheSumofOrders() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 20, "testAfteriTwoCompatibleOrdersRevenueIsTheSumofOrders" ) {}
 void runTest() { suite_TestLags.testAfteriTwoCompatibleOrdersRevenueIsTheSumofOrders(); }
} testDescription_suite_TestLags_testAfteriTwoCompatibleOrdersRevenueIsTheSumofOrders;

static class TestDescription_suite_TestLags_testAfteriTwoIncompatibleOrdersRevenueIsTheMaxofOrders : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_testAfteriTwoIncompatibleOrdersRevenueIsTheMaxofOrders() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 27, "testAfteriTwoIncompatibleOrdersRevenueIsTheMaxofOrders" ) {}
 void runTest() { suite_TestLags.testAfteriTwoIncompatibleOrdersRevenueIsTheMaxofOrders(); }
} testDescription_suite_TestLags_testAfteriTwoIncompatibleOrdersRevenueIsTheMaxofOrders;

static class TestDescription_suite_TestLags_testAfterSeveralCompatibleOrIncompatibleOrdersRevenueIsTheBestofOrders : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_testAfterSeveralCompatibleOrIncompatibleOrdersRevenueIsTheBestofOrders() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 34, "testAfterSeveralCompatibleOrIncompatibleOrdersRevenueIsTheBestofOrders" ) {}
 void runTest() { suite_TestLags.testAfterSeveralCompatibleOrIncompatibleOrdersRevenueIsTheBestofOrders(); }
} testDescription_suite_TestLags_testAfterSeveralCompatibleOrIncompatibleOrdersRevenueIsTheBestofOrders;

static class TestDescription_suite_TestLags_testAfterSeveralCompatibleOrIncompatibleUnsortedOrdersRevenueIsTheBestofOrders : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_testAfterSeveralCompatibleOrIncompatibleUnsortedOrdersRevenueIsTheBestofOrders() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 43, "testAfterSeveralCompatibleOrIncompatibleUnsortedOrdersRevenueIsTheBestofOrders" ) {}
 void runTest() { suite_TestLags.testAfterSeveralCompatibleOrIncompatibleUnsortedOrdersRevenueIsTheBestofOrders(); }
} testDescription_suite_TestLags_testAfterSeveralCompatibleOrIncompatibleUnsortedOrdersRevenueIsTheBestofOrders;

#include <cxxtest/Root.cpp>
const char* CxxTest::RealWorldDescription::_worldName = "cxxtest";
