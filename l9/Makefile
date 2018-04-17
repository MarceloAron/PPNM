CFLAGS = -Wall -std=gnu99 `gsl-config --cflags`
LDLIBS = `gsl-config --libs`

plot.svg: plot.gpi data
	gnuplot $<

data: main
	./$< > $@

main: main.o gamma_function.o

clean:
	$(RM) main *.o *.svg data
