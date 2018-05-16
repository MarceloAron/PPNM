FFLAGS = -ffree-form -std=gnu -O
LDLIBS = -lgfortran -lm
FC = gfortran

all:out.txt
	cat $<
out.txt:main
	./$< > $@

%.o %.mod: %.f
	$(FC) $(FFLAGS) -c $<
	touch $@

main:main.o roots.o qr_givens.o
main.o: roots.mod
roots.o: qr_givens.mod
qr_givens.o qr_givens.mod: qr_givens.f90
	$(FC) $(FFLAGS) -c $<

clean:
	find . -type f -executable -delete
	rm -f *.o *.mod out*
