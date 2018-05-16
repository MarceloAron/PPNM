all: out.txt
	cat out.txt

out.txt: A.txt B.txt
	cat A.txt B.txt | cat > out.txt

A.txt: A
	./A > A.txt

B.txt: B
	./B > B.txt

B: B.o roots.o qr_givens.o
	gfortran B.o roots.o qr_givens.o -o B

B.o: B.f90 roots.mod
	gfortran -c B.f90

A: A.o roots.o qr_givens.o
	gfortran A.o roots.o qr_givens.o -o A

A.o: A.f90 roots.mod
	gfortran -c A.f90

roots.o roots.mod: roots.f90 qr_givens.mod
	gfortran -c roots.f90

qr_givens.o qr_givens.mod: qr_givens.f90
	gfortran -c qr_givens.f90

clean:
	find . -type f -executable -delete
	rm -f *.txt *.mod *.o *~
