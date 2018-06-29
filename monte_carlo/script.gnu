set ylabel "Error"
set xlabel "Number of points"
set xrange [0:1000]
set title "Error estimate as a function of number of points"

f(x) = a + b/sqrt(x)
fit f(x) 'data.txt' via a,b
title_f(a,b) = sprintf('f(x) = %.2f + %.2f/sqrt(x)', a,b)

plot 'data.txt' pt 7 ps 0.5 lc rgb 'red' t "Error estimates for a certain N", f(x) w l lc rgb 'black' t title_f(a,b)

set term svg size 800,600 background "white" font "Times,18"
set out 'plot.svg'
replot