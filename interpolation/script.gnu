set ylabel 'y'
set xlabel 'x'
set key bottom

plot 'data.txt' i 0 pt 7 ps 0.5 lc rgb 'black' t 'Data points', 'data.txt' i 1 u 1:2 w l lc rgb 'red' t 'Linear spline', 'data.txt' i 1 u 1:3 w l lc rgb 'blue' t 'Quadratic spline', 'data.txt' i 1 u 1:4 w l lc rgb 'purple' t 'Cubic spline', 'data.txt' i 0 smooth csplines lc rgb 'gold' t 'Gnuplot cubic spline'

set term svg size 800,600 background rgb "white" font 'Times,18' fsize 18
set out 'splines.svg'
replot
