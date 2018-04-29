all: out.txt
	cat out.txt

out.txt: A1.txt A2.txt B.txt C.txt
	cat A1.txt A2.txt B.txt C.txt | cat > out.txt

C.txt: C
	./C > C.txt

C: C.o qr_givens.o
	gfortran C.o qr_givens.o -o C

C.o: C.f90 qr_givens.mod
	gfortran -c C.f90

B.txt: B
	./B > B.txt

B: B.o qr_gramschmidt.o
	gfortran B.o qr_gramschmidt.o -o B

B.o: B.f90 qr_gramschmidt.mod
	gfortran -c B.f90

A2.txt: A2
	./A2 > A2.txt

A2: A2.o qr_gramschmidt.o
	gfortran A2.o qr_gramschmidt.o -o A2

A2.o: A2.f90 qr_gramschmidt.mod
	gfortran -c A2.f90

A1.txt: A1
	./A1 > A1.txt

A1: A1.o qr_gramschmidt.o
	gfortran A1.o qr_gramschmidt.o -o A1

A1.o: A1.f90 qr_gramschmidt.mod
	gfortran -c A1.f90

qr_gramschmidt.o qr_gramschmidt.mod: qr_gramschmidt.f90
	gfortran -c qr_gramschmidt.f90

qr_givens.o qr_givens.mod: qr_givens.f90
	gfortran -c qr_givens.f90

clean:
	rm -f A1 A2 B C *.o *.mod *~ *.txt
