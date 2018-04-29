all: splines.eps intder.eps

intder.eps: data.txt script2.gnu
	gnuplot script2.gnu

splines.eps: data.txt script.gnu
	gnuplot script.gnu

data.txt: main
	./main > data.txt

main: main.o linspline.o quadspline.o cubespline.o
	gfortran main.o linspline.o quadspline.o cubespline.o -o main

main.o: main.f90 linspline.mod quadspline.mod cubespline.mod
	gfortran -c main.f90

linspline.o linspline.mod: linspline.f90
	gfortran -c linspline.f90

quadspline.o quadspline.mod: quadspline.f90
	gfortran -c quadspline.f90

cubespline.o cubespline.mod: cubespline.f90
	gfortran -c cubespline.f90

clean:
	rm -f main *.o *.mod *.txt *.svg *~
