set xlabel 'x'
set ylabel 'f(x)'
set yrange [-1:1.5]

plot 'io.dat' w lp pt 7 ps 0.2 lc rgb 'red' t 'Standard input: cos(x)', \
'cmd.dat' w lp pt 7 ps 0.2 lc rgb 'blue' t 'Command line argument: sin(x)'

set term svg size 800,600 background "white" font "Times,18"
set out 'plot.svg'
replot