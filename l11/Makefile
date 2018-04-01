CFLAGS = -Wall -std=gnu99 -O
CFLAGS += `gsl-config --cflags`
LDLIBS += $$(gsl-config --libs)
PLOTTER = gnuplot

all: plot.png log
	cat log

data log: main
	./$< 2>&1 > data | tee log

plot.png: plot.gpi data
	$(PLOTTER) $<

.PHONY: clean
clean: clean-test
	$(RM) data log main plot.png

.PHONY: clean-test
clean-test: ; $(RM) testd test.log

.PHONY: test
test: test.log ; cat $<
test.log: testd ; ./$< > $@
