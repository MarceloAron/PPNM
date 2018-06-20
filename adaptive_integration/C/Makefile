CFLAGS = -Wall -std=gnu11
LDLIBS = -lm

all:out.txt
	cat $<

out.txt:main
	./$< > $@ 2> check.txt

main: main.o adapt.o

clean:
	find . -type f -executable -delete
	rm -rf *.o out.txt
