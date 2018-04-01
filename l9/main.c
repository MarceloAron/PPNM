#include<stdio.h>
#include<math.h>
double gamma_function(double);

int main(){
	double a=-2*M_PI, b=2*M_PI, dx=0.013;
	for(double x=a;x<b;x+=dx)
		printf("%g %g %g\n",x,gamma_function(x),tgamma(x));
return 0;
}
