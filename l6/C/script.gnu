set xrange [0:12.56]
set yrange [-1.5:1.5]
set xlabel 'x'
set ylabel 'f(x)'
set grid

plot 'io.dat' w l lc rgb 'red' t 'standard input - cos(x)', 'cmd.dat' w l lc rgb 'blue' t 'command line - sin(x)'

set term svg size 800,600 font 'Times,18' fsize 18
set out 'plot.svg'
replot