all: l5.txt
	cat l5.txt

l5.txt: ex
	./ex > l5.txt

ex: mod.o ex.o
	gfortran mod.o ex.o -o ex

mod.o: mod.f90
	gfortran -c mod.f90

ex.o: ex.f90
	gfortran -c ex.f90

clean:
	rm -f ex *.o l5.txt *.mod
