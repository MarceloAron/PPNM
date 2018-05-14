set xlabel 'Matrix size'
set ylabel 'Diagonalization time (sec)'
set title "Diagonalitazion time as a function of matrix size (cyclic method)"
set key left

f(x) = (x/a)**p
a = 10
p = 3
fit f(x) 'A.times.txt' via a,p
T = sprintf("Fit: (n/%3.0f)^{%3.1f}",a,p)

plot 'A.times.txt' pt 7 ps 0.5 lc rgb 'red' t 'Measurement', f(x) w l lc rgb 'black' t T

set term svg size 800,600 background 'white' font 'Times,18'
set out 'cyclic.svg'
replot
