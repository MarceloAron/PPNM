#include<gsl/gsl_linalg.h>
#include<gsl/gsl_blas.h>
#include<stdio.h>
#include<stdlib.h>
#define RND (double)rand()/RAND_MAX
void matrix_print(const char* s,const gsl_matrix* m){
  printf(s); printf("\n");
  for(int j=0;j<m->size2;j++){
    for(int i=0;i<m->size1;i++)
      printf("%8.3g ",gsl_matrix_get(m,i,j));
    printf("\n");
  }
}
void vector_print(const char* s,const gsl_vector* v){
  printf(s); printf("\n");
  for(int i=0;i<v->size;i++)
    printf("%8.3g ",gsl_vector_get(v,i));
  printf("\n");
}
int main(){
  int n=3;
  gsl_matrix* M=gsl_matrix_calloc(n,n);
  gsl_matrix_set(M,0,0,6.13);
	gsl_matrix_set(M,1,0,-2.90);
	gsl_matrix_set(M,2,0,5.86);
	gsl_matrix_set(M,0,1,8.08);
	gsl_matrix_set(M,1,1,-6.31);
	gsl_matrix_set(M,2,1,-3.89);
	gsl_matrix_set(M,0,2,-4.36);
	gsl_matrix_set(M,1,2,1.00);
	gsl_matrix_set(M,2,2,0.19);
  gsl_vector* b=gsl_vector_calloc(n);
  gsl_vector_set(b,0,6.23);
	gsl_vector_set(b,1,5.37);
	gsl_vector_set(b,2,2.29);	
  gsl_matrix* A=gsl_matrix_calloc(n,n);
  gsl_matrix_memcpy(A,M);
  matrix_print("matrix M=",M);
  vector_print("right hand side b=",b);
  gsl_vector* x=gsl_vector_calloc(n);
  gsl_linalg_HH_solve(M,b,x);
  vector_print("solution x to Mx=b :",x);
  gsl_vector* y=gsl_vector_calloc(n);
  gsl_blas_dgemv(CblasNoTrans,1,A,x,0,y);
  vector_print("check:  Mx= (should be equal b)",y);
  /* fixme: free memory */
  return 0;
}
