set xlabel 'x'
set ylabel 'y'
set xrange [0:7]
set yrange [-1.1:1.6]
set title "Integrating d^2u/dt^2 = -u with RK4 and Runge's principle"

plot 'data.txt' u 1:2 w lp pt 7 ps 0.5 lc rgb 'red' t 'y(x) (should be sin)',\
     'data.txt' u 1:3 w lp pt 7 ps 0.5 lc rgb 'blue' t "'y'(x) (should be cos)'",\
     sin(x) w l lc rgb 'black',\
     cos(x) w l lc rgb 'grey'

set term postscript eps enhanced colour "Helvetica" 14
set out 'plot.eps'
replot