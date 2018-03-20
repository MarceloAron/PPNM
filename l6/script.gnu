set xrange [0:16]
set yrange [-1.2:1.2]
set xlabel 'x'
set ylabel 'f(x)'

plot 'io.dat' w l lc rgb 'red' t 'standard input - cos(x)', 'cmd.dat' w l lc rgb 'blue' t 'command line - sin(x)'

set term postscript eps enhanced colour "Helvetica" 14
set out 'plot.eps'
replot