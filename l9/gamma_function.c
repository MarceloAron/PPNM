#include<math.h>
#include<gsl/gsl_integration.h>
#include<gsl/gsl_errno.h>

double gamma_integrand(double x, void* params){
	double z = *(double*)params;
	return pow(x,z-1)*exp(-x);
}

double gamma_function(double z){
	if(z<0) return M_PI/sin(M_PI*z)/gamma_function(1-z);

	gsl_function f;
	f.function = gamma_integrand;
	f.params = (void*)&z;

	int limit = 100;
	double a=0,acc=1e-6,eps=1e-6,result,err;
	gsl_integration_workspace * workspace =
		gsl_integration_workspace_alloc(limit);
int status=gsl_integration_qagiu(&f,a,acc,eps,limit,workspace,&result,&err);
	gsl_integration_workspace_free(workspace);
	if(status!=GSL_SUCCESS) return NAN;
	else return result;
}
