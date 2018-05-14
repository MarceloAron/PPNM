all: AB.svg C.svg

AB.svg: script.gnu AB.txt
	gnuplot script.gnu

C.svg: script2.gnu C.txt
	gnuplot script2.gnu

AB.txt: AB
	./AB > AB.txt

C.txt: C
	./C > C.txt

AB: AB.o least_squares.o qr_givens.o jacobi.o
	gfortran AB.o least_squares.o qr_givens.o jacobi.o -o AB

AB.o: AB.f90 least_squares.mod
	gfortran -c AB.f90

C: C.o least_squares.o jacobi.o qr_givens.o
	gfortran C.o least_squares.o jacobi.o  qr_givens.o -o C

C.o: C.f90 least_squares.mod
	gfortran -c C.f90

least_squares.o least_squares.mod: least_squares.f90 qr_givens.mod jacobi.mod
	gfortran -c least_squares.f90

qr_givens.o qr_givens.mod: qr_givens.f90
	gfortran -c qr_givens.f90

jacobi.o jacobi.mod: jacobi.f90
	gfortran -c jacobi.f90

clean:
	find . -type f -executable -delete
	rm -f *.txt *.o *.mod *.svg *~

