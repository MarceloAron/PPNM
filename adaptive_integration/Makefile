all: out.txt
	cat out.txt

out.txt: main
	./main > out.txt 2> check.txt

main: main.o integration.o
	gfortran main.o integration.o -o main

main.o: main.f90 integration.mod
	gfortran -c main.f90

integration.o integration.mod: integration.f90
	gfortran -c integration.f90

clean:
	find . -type f -executable -delete
	rm -f *.o *.mod *.txt *~
