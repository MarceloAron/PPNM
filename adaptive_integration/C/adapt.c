#include<math.h>
#include<assert.h>
#include<stdio.h>
double adapt24
	(double f(double),double a, double b,
	double acc, double eps, double f2, double f3, int nrec)
{
	assert(nrec<1000000);
	double f1=f(a+(b-a)/6), f4=f(a+5*(b-a)/6);
	double Q=(2*f1+f2+f3+2*f4)/6*(b-a), q=(f1+f4+f2+f3)/4*(b-a);
	double tolerance=acc+eps*fabs(Q), error=fabs(Q-q)/2;
	fprintf( stderr, "Q= %g, q=%g, err=%g, tol=%g, nrec=%d\n",Q,q,error,tolerance,nrec);
	if(error < tolerance) {
	  fprintf( stderr, "Accepted\n");
	  return Q;
	}	  
	else {
//              fprintf( stderr, "before nrec=%d\n",nrec);
	        double Q1=adapt24(f,a,(a+b)/2,acc/sqrt(2.),eps,f1,f2,nrec+1);
		double Q2=adapt24(f,(a+b)/2,b,acc/sqrt(2.),eps,f3,f4,nrec+1);
//              fprintf( stderr, "after nrec=%d\n",nrec);			  
		return Q1+Q2;
	}
}

double adapt
	(double f(double),double a,double b,
	double acc,double eps)
{
	double f2=f(a+2*(b-a)/6),f3=f(a+4*(b-a)/6);
	int nrec=0;
	return adapt24(f,a,b,acc,eps,f2,f3,nrec);
}

double clenshaw_curtis(double f(double),double a,double b,double acc,double eps){
	double g(double t){return f( (a+b)/2+(a-b)/2*cos(t) )*sin(t)*(b-a)/2;}
	return adapt(g,0,M_PI,acc,eps);
}
