all: out.txt plot.svg
	cat out.txt

plot.svg: data.txt script.gnu
	gnuplot script.gnu

out.txt: main
	./main > out.txt 2>> data.txt

main: main.o montecarlo.o integration.o
	gfortran main.o montecarlo.o integration.o -o main

main.o: main.f90 montecarlo.mod integration.mod
	gfortran -c main.f90

montecarlo.o montecarlo.mod: montecarlo.f90
	gfortran -c montecarlo.f90

integration.o integration.mod: integration.f90
	gfortran -c integration.f90

clean:
	find . -type f -executable -delete
	rm -f *.o *.mod *.txt *~ *.log *.svg
