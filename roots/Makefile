all: out.txt
	cat out.txt

out.txt: AB
	./AB > out.txt

AB: AB.o roots.o qr_givens.o
	gfortran AB.o roots.o qr_givens.o -o AB

AB.o: AB.f90 roots.mod
	gfortran -c AB.f90

roots.o roots.mod: roots.f90 qr_givens.mod
	gfortran -c roots.f90

qr_givens.o qr_givens.mod: qr_givens.f90
	gfortran -c qr_givens.f90

clean:
	find . -type f -executable -delete
	rm -f *.txt *.mod *.o *~
