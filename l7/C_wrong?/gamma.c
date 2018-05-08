#include<stdio.h>
#include<gsl/gsl_sf_gamma.h>
#include<math.h>
int main(){
  for(double x=-2*M_PI;x<2*M_PI;x+=0.013)
    printf("%g %g %g\n",x,gsl_sf_gamma(x),tgamma(x));
  return 0;
}
