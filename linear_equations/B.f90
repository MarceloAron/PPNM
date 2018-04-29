program exB
  use qr_gramschmidt
  implicit none

  real*8, allocatable :: A(:,:), R(:,:), B(:,:), C(:,:)
  integer n

  !  read*, n
  n = 3

  allocate(A(n,n),R(n,n),B(n,n),C(n,n))

  write(*,*) "===================================="
  write(*,*) "             Exercise B             "
  write(*,*) "===================================="

  call random_number(A)
  C(:,:) = A(:,:)

  write(*,*) "Matrix A="
  call matrix_print(A)

  call qr_decomp(A,R)

  call matrix_inverse(A,R,B)

  write(*,*) "Matrix B="
  call matrix_print(B)

  write(*,*) "Check: is AB = I?"
  call matrix_print(matmul(C,B))

end program exB

