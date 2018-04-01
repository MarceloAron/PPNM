#include<stdio.h>
#include<math.h>
#include<stdlib.h>
#include<gsl/gsl_randist.h>
#define SIGMAREL 0.03
#define SIGMAACC 0.03

int main(){
	gsl_rng *rnd = gsl_rng_alloc(gsl_rng_default);
	for(int i=0;i<99;i++) {double dummy = gsl_ran_gaussian(rnd,1);}
	int n=10;
	double A=5,T=3,B=1,t2=M_PI*T,dt=t2/n;
	double t[n],y[n],e[n];
	for(int i=0;i<n;i++) t[i]=dt/2+dt*i;
	for(int i=0;i<n;i++){
		double datum = A*exp(-t[i]/T) + B;
		double relerr = gsl_ran_gaussian(rnd,SIGMAREL)*datum;
		double abserr = gsl_ran_gaussian(rnd,SIGMAACC);
		double err = SIGMAACC + SIGMAREL + sqrt(abserr*abserr+relerr*relerr);
		y[i]=datum+relerr+abserr;
		e[i]=err;
		}
	printf("# t_i   y_i   e_i\n");
	for(int i=0;i<n;i++) printf("%5.2f %5.2f %5.2f\n",t[i],y[i],e[i]);
	printf("\ndouble t[]= {%4.2f",t[0]);
	for(int i=1;i<n;i++) printf(",%4.2f",t[i]);
	printf("};");
	printf("\ndouble y[]= {%4.2f",y[0]);
	for(int i=1;i<n;i++) printf(",%4.2f",y[i]);
	printf("};");
	printf("\ndouble e[]= {%4.2f",e[0]);
	for(int i=1;i<n;i++) printf(",%4.2f",e[i]);
	printf("};");

	printf("\n");
	
gsl_rng_free(rnd);
return 0;
}
