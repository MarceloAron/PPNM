all: report.pdf

report.pdf: report.tex plot.pdf
	pdflatex report.tex

plot.pdf: script.gnu data.dat
	gnuplot script.gnu

data.dat: ex
	./ex > data.dat

ex: ex.f90
	gfortran ex.f90 -o ex

clean:
	rm -f *.dat ex *.eps *.log *.out *.aux *.pdf
