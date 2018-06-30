all: out.txt plot.svg
	cat out.txt

plot.svg: data.txt
	echo 'set term svg size 800,600 background "white" font "Times,18";\
	set out "$@";\
	set title "E vs. alpha";\
	set xrange [0:2];\
	set yrange [0.4:1];\
	set xlabel "alpha";\
	set ylabel "E";\
	plot "$<" w l lc rgb "red" notitle;\
	' | gnuplot

out.txt data.txt: main
	./main > out.txt 2> data.txt

main: main.o integration.o
	gfortran main.o integration.o -o main

main.o: main.f90 integration.mod
	gfortran -c main.f90

integration.o integration.mod: integration.f90
	gfortran -c integration.f90

clean:
	find . -type f -executable -delete
	rm -f *.o *.mod *.txt *~

