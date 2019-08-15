rent: rent.c
	cc rent.c -o rent

test: rent tests.txt
	sed -n -e 's/^\(< \)\(.*\)/\2/pw input.dat'      tests.txt >/dev/null
	sed -n -e 's/^\(> \)\(.*\)/\2/pw expected.dat'   tests.txt >/dev/null
	./rent <input.dat >result.dat
	diff expected.dat result.dat

clean:
	rm rent; rm *.dat

large: rent.c
	./rent <largedata.txt >result.dat
	diff largeresult.txt result.dat

