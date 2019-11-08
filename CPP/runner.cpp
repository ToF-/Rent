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
#include "test_lags.h"

static TestLags suite_TestLags;

static CxxTest::List Tests_TestLags = { 0, 0 };
CxxTest::StaticSuiteDescription suiteDescription_TestLags( "test_lags.h", 6, "TestLags", suite_TestLags, Tests_TestLags );

static class TestDescription_suite_TestLags_testWhenNoOrderGivenRevenueIsZero : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_testWhenNoOrderGivenRevenueIsZero() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 9, "testWhenNoOrderGivenRevenueIsZero" ) {}
 void runTest() { suite_TestLags.testWhenNoOrderGivenRevenueIsZero(); }
} testDescription_suite_TestLags_testWhenNoOrderGivenRevenueIsZero;

#include <cxxtest/Root.cpp>
const char* CxxTest::RealWorldDescription::_worldName = "cxxtest";
