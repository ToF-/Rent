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
CxxTest::StaticSuiteDescription suiteDescription_TestLags( "test_rent.h", 13, "TestLags", suite_TestLags, Tests_TestLags );

static class TestDescription_suite_TestLags_test_When_No_Order_Given_Revenue_Is_Zero : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_test_When_No_Order_Given_Revenue_Is_Zero() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 16, "test_When_No_Order_Given_Revenue_Is_Zero" ) {}
 void runTest() { suite_TestLags.test_When_No_Order_Given_Revenue_Is_Zero(); }
} testDescription_suite_TestLags_test_When_No_Order_Given_Revenue_Is_Zero;

static class TestDescription_suite_TestLags_test_after_one_order_revenue_is_this_order : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_test_after_one_order_revenue_is_this_order() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 21, "test_after_one_order_revenue_is_this_order" ) {}
 void runTest() { suite_TestLags.test_after_one_order_revenue_is_this_order(); }
} testDescription_suite_TestLags_test_after_one_order_revenue_is_this_order;

static class TestDescription_suite_TestLags_test_after_two_compatible_orders_revenue_is_the_sumof_orders : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_test_after_two_compatible_orders_revenue_is_the_sumof_orders() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 27, "test_after_two_compatible_orders_revenue_is_the_sumof_orders" ) {}
 void runTest() { suite_TestLags.test_after_two_compatible_orders_revenue_is_the_sumof_orders(); }
} testDescription_suite_TestLags_test_after_two_compatible_orders_revenue_is_the_sumof_orders;

static class TestDescription_suite_TestLags_test_after_two_incompatible_orders_revenue_is_the_maxof_orders : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_test_after_two_incompatible_orders_revenue_is_the_maxof_orders() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 34, "test_after_two_incompatible_orders_revenue_is_the_maxof_orders" ) {}
 void runTest() { suite_TestLags.test_after_two_incompatible_orders_revenue_is_the_maxof_orders(); }
} testDescription_suite_TestLags_test_after_two_incompatible_orders_revenue_is_the_maxof_orders;

static class TestDescription_suite_TestLags_test_after_several_compatible_or_incompatible_orders_revenue_is_the_best_of_orders : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_test_after_several_compatible_or_incompatible_orders_revenue_is_the_best_of_orders() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 41, "test_after_several_compatible_or_incompatible_orders_revenue_is_the_best_of_orders" ) {}
 void runTest() { suite_TestLags.test_after_several_compatible_or_incompatible_orders_revenue_is_the_best_of_orders(); }
} testDescription_suite_TestLags_test_after_several_compatible_or_incompatible_orders_revenue_is_the_best_of_orders;

static class TestDescription_suite_TestLags_test_after_several_compatible_or_incompatible_unsorted_orders_revenue_is_the_best_of_orders : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_test_after_several_compatible_or_incompatible_unsorted_orders_revenue_is_the_best_of_orders() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 50, "test_after_several_compatible_or_incompatible_unsorted_orders_revenue_is_the_best_of_orders" ) {}
 void runTest() { suite_TestLags.test_after_several_compatible_or_incompatible_unsorted_orders_revenue_is_the_best_of_orders(); }
} testDescription_suite_TestLags_test_after_several_compatible_or_incompatible_unsorted_orders_revenue_is_the_best_of_orders;

static class TestDescription_suite_TestLags_test_Order_reader_should_read_an_order_from_a_stream : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_test_Order_reader_should_read_an_order_from_a_stream() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 59, "test_Order_reader_should_read_an_order_from_a_stream" ) {}
 void runTest() { suite_TestLags.test_Order_reader_should_read_an_order_from_a_stream(); }
} testDescription_suite_TestLags_test_Order_reader_should_read_an_order_from_a_stream;

static class TestDescription_suite_TestLags_test_revenue_write_should_write_a_revenue_into_a_stream : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_test_revenue_write_should_write_a_revenue_into_a_stream() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 68, "test_revenue_write_should_write_a_revenue_into_a_stream" ) {}
 void runTest() { suite_TestLags.test_revenue_write_should_write_a_revenue_into_a_stream(); }
} testDescription_suite_TestLags_test_revenue_write_should_write_a_revenue_into_a_stream;

static class TestDescription_suite_TestLags_test_session_should_read_a_list_of_orders_and_output_revenue : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_test_session_should_read_a_list_of_orders_and_output_revenue() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 75, "test_session_should_read_a_list_of_orders_and_output_revenue" ) {}
 void runTest() { suite_TestLags.test_session_should_read_a_list_of_orders_and_output_revenue(); }
} testDescription_suite_TestLags_test_session_should_read_a_list_of_orders_and_output_revenue;

static class TestDescription_suite_TestLags_test_session_should_read_a_list_of_cases_and_output_revenues : public CxxTest::RealTestDescription {
public:
 TestDescription_suite_TestLags_test_session_should_read_a_list_of_cases_and_output_revenues() : CxxTest::RealTestDescription( Tests_TestLags, suiteDescription_TestLags, 82, "test_session_should_read_a_list_of_cases_and_output_revenues" ) {}
 void runTest() { suite_TestLags.test_session_should_read_a_list_of_cases_and_output_revenues(); }
} testDescription_suite_TestLags_test_session_should_read_a_list_of_cases_and_output_revenues;

#include <cxxtest/Root.cpp>
const char* CxxTest::RealWorldDescription::_worldName = "cxxtest";
