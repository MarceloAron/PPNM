program main
  use qr_gramschmidt
  implicit none
  real*8 :: A(3,3), R(3,3), C(3,3), b(3), x(3), y(3)

  A(1,1) = 6.13
  A(1,2) =-2.90
  A(1,3) = 5.86 
  A(2,1) = 8.08
  A(2,2) =-6.31 
  A(2,3) =-3.89
  A(3,1) =-4.36 
  A(3,2) = 1.00
  A(3,3) = 0.19

  C(:,:) = A(:,:)

  b(1) = 6.23
  b(2) = 5.37
  b(3) = 2.29

  write(*,*) "Matrix A="
  call matrix_print(A)

  write(*,*) "Vector b="
  call vector_print(b)

  call qr_decomp(A,R)

  y = matmul(transpose(A),b)

  call qr_solve(R,x,y)

  write(*,*) "Vector x="
  call vector_print(x)

  write(*,*) "Check: is Ax = b?"
  call vector_print(matmul(C,x))

end program main
