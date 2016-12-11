rent : rent.c
	cc rent.c -o rent

test : rent test.dat expected.dat
	./rent <test.dat >result.dat; diff expected.dat result.dat
	
