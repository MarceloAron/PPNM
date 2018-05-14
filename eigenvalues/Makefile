all:out.txt cyclic.svg single_one.svg single_all.svg classic.svg
	cat out.txt

out.txt: A.txt B1.txt B2.txt C.txt
	cat A.txt B1.txt B2.txt C.txt | cat > out.txt
	rm A.txt B1.txt B2.txt C.txt

A.txt: A
	./A 4 > A.txt

A: A.o jacobi.o
	gfortran A.o jacobi.o -o A

A.o: A.f90 jacobi.mod
	gfortran -c A.f90

B1.txt: B1
	for m in `echo 1 2 4 `; do ./B1 4 $$m >> B1.txt; done

B1: B1.o jacobi.o
	gfortran B1.o jacobi.o -o B1

B1.o: B1.f90 jacobi.mod
	gfortran -c B1.f90

B2.txt: B2
	for m in `echo 1 2 4 `; do ./B2 4 $$m >> B2.txt; done

B2: B2.o jacobi.o
	gfortran B2.o jacobi.o -o B2

B2.o: B2.f90 jacobi.mod
	gfortran -c B2.f90

C.txt: C
	./C 4 > C.txt

C: C.o jacobi.o
	gfortran C.o jacobi.o -o C

C.o: C.f90 jacobi.mod
	gfortran -c C.f90

jacobi.o jacobi.mod: jacobi.f90
	gfortran -c jacobi.f90

cyclic.svg: A.times.txt script.gnu
	gnuplot script.gnu

single_one.svg: B.one.times.txt script2.gnu
	gnuplot script2.gnu

single_all.svg: B.all.times.txt script3.gnu
	gnuplot script3.gnu

classic.svg: C.times.txt script4.gnu
	gnuplot script4.gnu

A.times.txt: A Makefile
	cat /dev/null > $@
	for n in `seq 150 1 180`; do time --format "$$n %U" --append --output $@ ./A $$n 2>/dev/null; done

B.one.times.txt: B1 Makefile
	cat /dev/null > $@
	for n in `seq 150 1 180`; do time --format "$$n %U" --append --output $@ ./B1 $$n 1 2>/dev/null; done

B.all.times.txt: B1 Makefile
	cat /dev/null > $@
	for n in `seq 50 1 70`; do time --format "$$n %U" --append --output $@ ./B1 $$n 2>/dev/null; done

C.times.txt: C Makefile
	cat /dev/null > $@
	for n in `seq 20 1 40`; do time --format "$$n %U" --append --output $@ ./C $$n 2>/dev/null; done

clean:
	find . -type f -executable -delete
	rm -f *.o *.mod out* *.svg *~ *.txt *.log

