rent : rent.c
	cc rent.c -o rent

test : rent test.txt
	sed -n -e 's/\(< \)\(.*\)/\2/pw test.dat' -e 's/\(> \)\(.*\)/\2/pw expected.dat' test.txt >/dev/null
	./rent <test.dat >result.dat; diff expected.dat result.dat

clean : 
	rm rent;rm *.dat
	
