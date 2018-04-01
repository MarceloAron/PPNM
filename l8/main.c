#include<stdio.h>
#include<unistd.h> /* short comman-line options */
#include<getopt.h> /* long and short comman-line options */
#include<math.h>
#include<gsl/gsl_errno.h>
#include<gsl/gsl_odeiv2.h>

int orbital_equation
(double phi, const double y[], double yprime[], void *params)
{
double epsilon = *(double *) params;
yprime[0] = y[1];
yprime[1] = 1 - y[0] + epsilon * y[0] * y[0];
return GSL_SUCCESS;
}

int main (int argc, char **argv) {
	double epsilon = 0, uprime = 0;  /* default values */

/* reading parameters from short command-line options */
/*
while(1){
	int opt = getopt(argc, argv, "e:p:");
	if(opt == -1) break;
	switch(opt){
		case 'e': epsilon=atof(optarg); break;
		case 'p': uprime=atof(optarg); break;
		default:
                   fprintf(stderr, "Usage: %s [-e epsilon] [-p uprime]\n", argv[0]);
                   exit(EXIT_FAILURE);

	}
}
*/
/* end reading parameters from short command-line options */

/* reading epsilon and uprime from long command-line options */
while (1)
{
	struct option long_options[] =
	{
	{"epsilon", required_argument, NULL, 'e'},
	{"uprime" , required_argument, NULL, 'p'},
	{0, 0, 0, 0}
	};
	int opt = getopt_long (argc, argv, "e:p:", long_options, NULL);
	if( opt == -1 ) break;
	switch (opt) {
		case 'e': epsilon = atof (optarg); break;
		case 'p': uprime  = atof (optarg); break;
		default:
			fprintf (stderr, "Usage: %s --epsilon epsilon --uprime uprime\n", argv[0]);
			exit (EXIT_FAILURE);
		}
}
/* end of processing command-line options */

	gsl_odeiv2_system orbit;
	orbit.function = orbital_equation;
	orbit.jacobian = NULL;
	orbit.dimension = 2;
	orbit.params = (void *) &epsilon;

	double hstart = 1e-3, epsabs = 1e-6, epsrel = 1e-6;
	double phi_max = 39.5 * M_PI, delta_phi = 0.05;

	gsl_odeiv2_driver *driver =
		gsl_odeiv2_driver_alloc_y_new
			(&orbit, gsl_odeiv2_step_rk8pd, hstart, epsabs, epsrel);

	double t = 0, y[2] = { 1, uprime };
	for (double phi = 0; phi < phi_max; phi += delta_phi) {
		int status = gsl_odeiv2_driver_apply (driver, &t, phi, y);
		printf ("%g %g\n", phi, y[0]);
		if (status != GSL_SUCCESS) fprintf (stderr, "fun: status=%i", status);
		}

	gsl_odeiv2_driver_free (driver);
return EXIT_SUCCESS;
}
