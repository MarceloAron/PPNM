CFLAGS += -Wall -std=gnu1x $$(gsl-config --cflags)
LDLIBS += $$(gsl-config --libs)

.PHONEY:all
all: gamma.eps linear.out.txt
	cat $(word 2,$^)
gamma.eps:gamma.gnu gamma.data
	gnuplot $<
gamma.data:gamma
	./$< > $@
linear.out.txt:linear
	./$< > $@

.PHONEY:clean
clean:
	find . -type f -executable -delete
	rm -f *.o *.txt *.data *.eps
