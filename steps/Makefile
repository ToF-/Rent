rent : src/rent.c
	cc src/rent.c -o bin/rent

test : rent testdata/tests.txt
	sed -n -e 's/\(< \)\(.*\)/\2/pw input.dat' testdata/tests.txt >/dev/null
	sed -n -e 's/\(> \)\(.*\)/\2/pw expected.dat' testdata/tests.txt >/dev/null
	bin/rent <input.dat >result.dat
	diff expected.dat result.dat

clean : 
	rm bin/rent; rm *.dat
