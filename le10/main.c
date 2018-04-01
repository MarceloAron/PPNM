#include<stdio.h>
#include<assert.h>
#include<stdlib.h>
#include<gsl/gsl_multiroots.h>
#include<gsl/gsl_errno.h>
#define FSOLVER gsl_multiroot_fsolver_broyden

double Fe(double e, double r);

int master(const gsl_vector *x, void *params, gsl_vector *f){
	double e=gsl_vector_get(x,0);
	assert(e<0);
	double rmax=*(double*)params;
	double fval=Fe(e,rmax);
	gsl_vector_set(f,0,fval);
	return GSL_SUCCESS;
}

int main(int argc, char** argv){
	double rmax= argc>1? atof(argv[1]):10;
	fprintf(stderr,"rmax = %g\n",rmax);

	int dimension = 1;
	gsl_multiroot_fsolver * solver =
		gsl_multiroot_fsolver_alloc (FSOLVER, dimension);

	gsl_multiroot_function F = {.f=master, .n=dimension, .params=(void*)&rmax};

	gsl_vector *x = gsl_vector_alloc(dimension);
	gsl_vector_set(x,0,-1);

	gsl_multiroot_fsolver_set(solver, &F, x);

	int status, iter=0;
	const double epsabs=1e-3;
	do{
		iter++;
		status = gsl_multiroot_fsolver_iterate(solver);
		if(status)break;

		status = gsl_multiroot_test_residual(solver->f,epsabs);
		if(status==GSL_SUCCESS)fprintf(stderr,"converged\n");

		fprintf(stderr,"iter= %3i ",iter);
		fprintf(stderr,"e= %10g ",gsl_vector_get(solver->x,0));
		fprintf(stderr,"f(rmax)= %10g ",gsl_vector_get(solver->f,0));
		fprintf(stderr,"\n");
	}while( status == GSL_CONTINUE && iter < 100);

	double e=gsl_vector_get(solver->x,0);
	printf("# rmax, e\n");
	printf("%g %g\n",rmax,e);
	printf("\n\n");

	printf("# r, Fe(e,r), exact\n");
//	for(double r=0; r<=rmax; r+=rmax/64) printf("%g %g %g\n",r,Fe(e,r),r*exp(-r)*Fe(e,1)*exp(1));
	for(double r=0; r<=rmax; r+=rmax/64) printf("%g %g %g\n",r,Fe(e,r),r*exp(-r));

gsl_multiroot_fsolver_free(solver);
gsl_vector_free(x);
return EXIT_SUCCESS;
}
