test: test_rent.h rent.cpp
	clear
	cxxtestgen --error-printer -o runner.cpp test_rent.h
	g++ -o runner -I$CXXTEST runner.cpp	rent.cpp
	./runner

spoj: spoj.cpp rent.cpp
	clear
	g++ -o rent spoj.cpp 
