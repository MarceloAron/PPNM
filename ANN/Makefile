all: plot.svg

plot.svg: out.txt Makefile
	echo '\
	set term svg size 800,600 background rgb "white" font "Times,18";\
	set out "$@";\
	set title "Neural network curve fitting";\
	set xlabel "x";\
	set ylabel "y";\
	plot\
	 "$<" index 0 pt 7 ps 0.5 t "Data"\
	,"$<" index 1 w l lc rgb "black" t "Fit"\
	'|gnuplot

out.txt: main
	./main > check.txt 2> $@

main: main.o ann.o minimization.o qr_givens.o
	gfortran main.o ann.o minimization.o qr_givens.o -o main

main.o: main.f90 ann.mod minimization.mod qr_givens.mod
	gfortran -c main.f90

ann.o ann.mod: ann.f90 minimization.mod
	gfortran -c ann.f90

minimization.o minimization.mod: minimization.f90 qr_givens.mod
	gfortran -c minimization.f90

qr_givens.o qr_givens.mod: qr_givens.f90
	gfortran -c qr_givens.f90

clean:
	find . -type f -executable -delete
	rm -f *.txt *.o *.mod *.svg *~
