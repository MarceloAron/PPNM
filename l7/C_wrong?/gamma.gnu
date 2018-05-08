set term postscript eps enhanced colour "Helvetica" 14
set out "gamma.eps"
set xlabel "x"
set ylabel "y"
set key noenhanced
set key bottom
set grid
set tics out
plot [-5:5][-5:5] \
     	            "gamma.data" using 1:2 with lines title 'gsl_sf_gamma from GSL' \
      		   ,"gamma.data" using 1:3 with lines title "tgamma from math.h" \
		   
