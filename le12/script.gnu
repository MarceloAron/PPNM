set xlabel 'x'
set ylabel 'f(x)'
set key top left

plot 'data.dat' u 1:2 w l lc rgb 'red' t 'Numerical Euler', 'data.dat' u 1:3 w l lc rgb 'blue' t 'Analytical', 'data.dat' u 1:4 w l lc rgb 'gold' t 'Numerical Runge-Kutta'

set term pdf enhanced
set out 'plot.pdf'
replot