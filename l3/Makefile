all: ex1.txt ex2.txt ex3.txt ex4.txt
	cat ex1.txt ex2.txt ex3.txt ex4.txt

ex1.txt: ex1
	./ex1 > ex1.txt

ex2.txt: ex2
	./ex2 > ex2.txt

ex3.txt: ex3
	./ex3 > ex3.txt

ex4.txt: ex4
	./ex4 > ex4.txt

ex1: ex1.f90
	gfortran ex1.f90 -o ex1

ex2: ex2.f90
	gfortran ex2.f90 -o ex2

ex3: ex3.f90
	gfortran ex3.f90 -o ex3

ex4: ex4.f90
	gfortran ex4.f90 -o ex4

clean:
	rm -f ex1.txt ex2.txt ex3.txt ex4.txt ex1 ex2 ex3 ex4
