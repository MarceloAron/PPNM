program A1
  use qr_gramschmidt
  implicit none
  real*8, allocatable :: A(:,:), B(:,:), R(:,:)
  integer m, n

  !  read*, n, m
  n = 4
  m = 3

  write(*,*) "===================================="
  write(*,*) "            Exercise A.1            "
  write(*,*) "===================================="

  allocate(A(n,m), B(n,m), R(m,m))

  call random_number(A)
  
  B(:,:) = A(:,:)

  write(*,*) "Matrix A="
  call matrix_print(A)

  call qr_decomp(A,R)

  write(*,*) "Matrix Q="
  call matrix_print(A)

  write(*,*) "Matrix R="
  call matrix_print(R)

  write(*,*) "Check: is Q*R=A?"
  call matrix_print(matmul(A,R))

  write(*,*) "Check: is Q^T * Q = 1?"
  call matrix_print(matmul(transpose(A),A))

  write(*,*) "Check: is Q^T * A = R?"
  call matrix_print(matmul(transpose(A),B))

end program A1
