program C
  use qr_givens
  implicit none
  real*8, allocatable :: A(:,:), B(:,:), Q(:,:), ai(:,:), R(:,:), v(:), x(:)
  integer i, j, m, n

!  read*, n, m
  n = 4
  m = 3

  write(*,*) "===================================="
  write(*,*) "             Exercise C             "
  write(*,*) "===================================="

  write(*,*) "QR-Decomposition by Givens rotations:"
  allocate(A(n,m))
  call random_number(A)

  write(*,*) "Matrix A="
  call matrix_print(A)

  allocate(B(n,m))
  B(:,:) = A(:,:)

  call qr_givens_decomp(A)
  allocate(Q(n,m))
  call qr_givens_getq(A,Q)
  allocate(R(m,m))
  call qr_givens_getr(A,R)
 
  write(*,*) "Check: is Q*R=A?"
  call matrix_print(matmul(Q,R))

  write(*,*) "Check: is Q^T * Q = 1?"
  call matrix_print(matmul(transpose(Q),Q))
 
  write(*,*) "Matrix R="
  call matrix_print(R)
 
  write(*,*) "Check: is Q^T * A = R?"
  call matrix_print(matmul(transpose(Q),B))

  write(*,*) "Linear system of equations:"
  deallocate(A,B,Q,R)
  allocate(A(m,m),B(m,m),Q(m,m),R(m,m),v(m),x(m))
  call random_number(A)
  
  write(*,*) "Matrix A="
  call matrix_print(A)

  B(:,:) = A(:,:)
  call random_number(v)

  write(*,*) "Vector v="
  call vector_print(v)

  call qr_givens_decomp(A)
  x = qr_givens_solve(A,v)

  write(*,*) "Vector x="
  call vector_print(x)

  write(*,*) "Check: is A*x=v?"
  call vector_print(matmul(B,x))

  write(*,*) "Matrix inverse:"
  ai = qr_givens_inv(A)

  write(*,*) "Matrix A^-1="
  call matrix_print(ai)

  write(*,*) "Check: is A^-1 * A = 1?"
  call matrix_print(matmul(ai,b))

  write(*,*) "Check: is A*A^-1=1?"
  call matrix_print(matmul(b,ai))

end program C
