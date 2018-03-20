CFLAGS = -Wall -std=gnu99
LDLIBS = -lm
PLOTTER = gnuplot

all: plot.eps

plot.eps: script.gnu cmd.dat io.dat
	gnuplot script.gnu

cmd.dat: main-cmdline data.dat
	./main-cmdline `cat data.dat` > cmd.dat

main-cmdline: main-cmdline.c
	gcc main-cmdline.c -o main-cmdline -lm

io.dat: main-stdio data.dat
	cat data.dat | ./main-stdio > io.dat

main-stdio: main-stdio.c
	gcc main-stdio.c -o main-stdio -lm

data.dat: Makefile
	seq 0 0.1 16 > data.dat

clean:
	rm -f *.dat main-stdio main-cmdline plot.eps

