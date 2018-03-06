all: l1.txt
	cat l1.txt

l1.txt: l1
	./l1 > l1.txt

l1: l1.f90
	gfortran l1.f90 -o l1

clean:
	rm -f l1.txt l1
