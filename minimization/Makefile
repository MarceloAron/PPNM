all: out.txt
	cat out.txt

out.txt: AB
	./AB > out.txt

AB: AB.o minimization.o qr_givens.o
	gfortran AB.o minimization.o qr_givens.o -o AB

AB.o: AB.f90 minimization.mod
	gfortran -c AB.f90

minimization.o minimization.mod: minimization.f90 qr_givens.mod
	gfortran -c minimization.f90

qr_givens.o qr_givens.mod: qr_givens.f90
	gfortran -c qr_givens.f90

clean:
	find . -type f -executable -delete
	rm -f *.txt *.mod *.o *~
