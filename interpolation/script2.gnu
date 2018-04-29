set ylabel 'y'
set xlabel 'x'
set key top left

plot 'data.txt' i 1 u 1:5 w l lc rgb 'red' t 'Integral linear', 'data.txt' i 1 u 1:6 w l lc rgb 'blue' t 'Integral quadratic', 'data.txt' i 1 u 1:7 w l lc rgb 'purple' t 'Integral cubic', 'data.txt' i 1 u 1:8 w l lc rgb 'light-blue' t 'Derivative quadratic', 'data.txt' i 1 u 1:9 w l lc rgb 'magenta' t 'Derivative cubic'

set term svg size 800,600 background rgb "white" font 'Times,18' fsize 18
set out 'intder.svg'
replot
