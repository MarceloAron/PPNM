all: plot.svg

plot.svg: script.gnu io.dat cmd.dat
	gnuplot script.gnu

io.dat: stdio data.dat
	for n in `wc -l data.dat | awk '{print $$1}'`; do ./stdio $$n > io.dat; done

stdio: stdio.f90
	gfortran stdio.f90 -o stdio

cmd.dat: cmdline data.dat
	for n in `cat data.dat`; do ./cmdline $$n >> cmd.dat; done

cmdline: cmdline.f90
	gfortran cmdline.f90 -o cmdline

data.dat: Makefile
	seq 0 0.2 20 | cat > data.dat

clean:
	find . -type f -executable -delete
	rm -f *.dat *.svg *~
