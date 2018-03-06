all: l2_ex1.txt l2_ex2.txt
	cat l2_ex1.txt l2_ex2.txt

l2_ex1.txt: ex1
	./ex1 > l2_ex1.txt

l2_ex2.txt: ex2
	./ex2 > l2_ex2.txt

ex1: ex1.f90
	gfortran ex1.f90 -o ex1

ex2: ex2.f90
	gfortran ex2.f90 -o ex2

clean:
	rm -f l2_ex1.txt l2_ex2.txt ex1 ex2 
