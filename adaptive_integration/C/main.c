#include<math.h>
#include<assert.h>
#include<stdio.h>
double adapt
	(double f(double),double a,double b,double acc,double eps);
double clenshaw_curtis
	(double f(double),double a,double b,double acc,double eps);

int main() //uses gcc nested functions
{

	int calls=0;
	double a=0,b=1,acc=1e-4,eps=1e-4;
	double s(double x){calls++; return sqrt(x);}; //nested function
	calls=0;
	double Q=adapt(s,a,b,acc,eps);
	double exact=2./3;
	printf("open4: integrating sqrt(x) from %g to %g\n",a,b);
	printf("acc=%g eps=%g\n",acc,eps);
	printf("              Q = %g\n",Q);
	printf("          exact = %g\n",exact);
	printf("          calls = %d\n",calls);
	printf("estimated error = %g\n",acc+fabs(Q)*eps);
	printf("   actual error = %g\n",fabs(Q-exact));

/*	calls=0;
	Q=clenshaw_curtis(s,a,b,acc,eps);
	exact=2./3;
	printf("\nclenshaw_curtis: integrating sqrt(x) from %g to %g\n",a,b);
	printf("acc=%g eps=%g\n",acc,eps);
	printf("              Q = %g\n",Q);
	printf("          exact = %g\n",exact);
	printf("          calls = %d\n",calls);
	printf("estimated error = %g\n",acc+fabs(Q)*eps);
	printf("   actual error = %g\n",fabs(Q-exact));
*/

	calls=0;
	a=0,b=1,acc=1e-4,eps=1e-4;
	double f(double x){calls++; return 1/sqrt(x);}; //nested function
	calls=0;
	Q=adapt(f,a,b,acc,eps);
	exact=2;
	printf("\nopen4: integrating 1/sqrt(x) from %g to %g\n",a,b);
	printf("acc=%g eps=%g\n",acc,eps);
	printf("              Q = %g\n",Q);
	printf("          exact = %g\n",exact);
	printf("          calls = %d\n",calls);
	printf("estimated error = %g\n",acc+fabs(Q)*eps);
	printf("   actual error = %g\n",fabs(Q-exact));

/*	calls=0;
	Q=clenshaw_curtis(f,a,b,acc,eps);
	exact=2;
	printf("\nclenshaw_curtis: integrating 1/sqrt(x) from %g to %g\n",a,b);
	printf("acc=%g eps=%g\n",acc,eps);
	printf("              Q = %g\n",Q);
	printf("          exact = %g\n",exact);
	printf("          calls = %d\n",calls);
	printf("estimated error = %g\n",acc+fabs(Q)*eps);
	printf("   actual error = %g\n",fabs(Q-exact));
*/
	a=0,b=1,acc=0.001,eps=0.001;
	double f2(double x){calls++; return log(x)/sqrt(x);}; //nested function
	calls=0;
	Q=adapt(f2,a,b,acc,eps);
	exact=-4;
	printf("\nopen4: integrating log(x)/sqrt(x) from %g to %g\n",a,b);
	printf("acc=%g eps=%g\n",acc,eps);
	printf("              Q = %g\n",Q);
	printf("          exact = %g\n",exact);
	printf("          calls = %d\n",calls);
	printf("estimated error = %g\n",acc+fabs(Q)*eps);
	printf("   actual error = %g\n",fabs(Q-exact));
/*
	calls=0;
	Q=clenshaw_curtis(f2,a,b,acc,eps);
	exact=-4;
	printf("\nclenshaw_curtis: integrating log(x)/sqrt(x) from %g to %g\n",a,b);
	printf("acc=%g eps=%g\n",acc,eps);
	printf("              Q = %g\n",Q);
	printf("          exact = %g\n",exact);
	printf("          calls = %d\n",calls);
	printf("estimated error = %g\n",acc+fabs(Q)*eps);
	printf("   actual error = %g\n",fabs(Q-exact));
*/
return 0 ;
}
