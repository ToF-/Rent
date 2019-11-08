test: test_lags.h lags.cpp
	clear
	cxxtestgen --error-printer -o runner.cpp test_lags.h
	g++ -o runner -I$CXXTEST runner.cpp	lags.cpp
	./runner
