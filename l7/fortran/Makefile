all: out.txt
	cat out.txt

out.txt: main
	./main > out.txt

main: main.o qr_gramschmidt.o
	gfortran main.o qr_gramschmidt.o -o main

main.o: main.f90 qr_gramschmidt.mod
	gfortran -c main.f90

qr_gramschmidt.o qr_gramschmidt.mod: qr_gramschmidt.f90
	gfortran -c qr_gramschmidt.f90

clean:
	rm -f main *.o *.mod out.txt *~
