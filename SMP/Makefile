FFLAGS = -ffree-form -fimplicit-none -std=gnu -O
LDLIBS = -lgfortran -lm
FFLAGS += -fopenmp
LDLIBS += -lgomp
FC = gfortran

all: out.txt
	cat out.txt

out.txt: main Makefile
	./$< > $@

main: main.o integration.o
	gfortran -fopenmp main.o integration.o -o main

main.o: main.f90 integration.mod
	gfortran -fopenmp -c main.f90

integration.o integration.mod: integration.f90
	gfortran -c integration.f90

#%.o %.mod: %.f90
#	$(FC) $(FFLAGS) -c $<

#main: main.o ode.o

clean:
	find . -type f -executable -delete
	rm -f *.o *.mod out* *log *~ *.txt *.svg
