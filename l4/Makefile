all:l4.txt
	cat l4.txt

l4.txt:ex
	./ex > l4.txt

ex: mod.o ex.o
	gfortran mod.o ex.o -o ex

mod.o: mod.f90
	gfortran -c mod.f90

ex.o:ex.f90
	gfortran -c ex.f90

clean:
	rm -f ex *.o l4.txt *.mod
