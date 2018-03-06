all: ex1.txt ex2.txt
	cat ex1.txt ex2.txt

ex1.txt: ex1
	./ex1 > ex1.txt

ex2.txt: ex2
	./ex2 > ex2.txt

ex1: ex1.f90
	gfortran ex1.f90 -o ex1

ex2: ex2.f90
	gfortran ex2.f90 -o ex2

clean:
	rm -f ex1.txt ex2.txt ex1 ex2 
