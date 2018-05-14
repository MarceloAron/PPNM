set xlabel 'x'
set ylabel 'y'
set key bottom
set title "Least Squares fit of F_c(x) = c_1*log(x) + c_2 + c_3*x to some data (using SVD)"

plot 'C.txt' index 0 with errorbars pt 7 ps 0.5 lc rgb 'black' t 'data', \
'C.txt' index 1 using 1:2 w l lc rgb 'red' t 'F_c(x)', \
'C.txt' index 1 using 1:3 w l lc rgb 'blue' t 'F_c(x)_{c1+-dc1}', \
'C.txt' index 1 using 1:4 w l lc rgb 'gold' t 'F_c(x)_{c2+-dc2}', \
'C.txt' index 1 using 1:5 w l lc rgb 'purple' t 'F_c(x)_{c3+-dc3}', \
'C.txt' index 1 using 1:6 w l lc rgb 'blue' notitle, \
'C.txt' index 1 using 1:7 w l lc rgb 'gold' notitle, \
'C.txt' index 1 using 1:8 w l lc rgb 'purple' notitle 

set term svg size 800,600 background "white" font "Times,18"
set out 'C.svg'
replot
