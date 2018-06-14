all: plot.svg out.txt

plot.svg: data.txt Makefile
	echo 'set term svg size 800,600 background rgb "white" font "times,14";\
	set out "$@";\
	set xlabel "x";\
	set xrange [0:7];\
	set ylabel "y";\
	set yrange [-1.1:1.6];\
	set title "Integrating d^2u/dt^2 = -u with RK4 and Runge principle";\
	plot\
	"$<" u 1:2 w lp pt 7 ps 0.5 lc rgb "red" t "y(1) (should be sin)",\
	"$<" u 1:3 w lp pt 7 ps 0.5 lc rgb "blue" t "y(2) (should be cos)",\
	sin(x) w l lc rgb "black",\
	cos(x) w l lc rgb "grey"\
	'|gnuplot

out.txt data.txt: main
	./main > data.txt 2> out.txt

main: main.o ode.o
	gfortran main.o ode.o -o main

main.o: main.f90 ode.mod
	gfortran -c main.f90

ode.o ode.mod: ode.f90
	gfortran -c ode.f90

clean:
	find . -type f -executable -delete
	rm -f *.o *.mod *.txt *~ *.svg
