test: test_rent.h rent.cpp
	clear
	cxxtestgen --error-printer -o runner.cpp test_rent.h
	g++ -o runner -I$CXXTEST runner.cpp	rent.cpp
	./runner

main: rent.cpp
	clear
	g++ -o rent rent.cpp
