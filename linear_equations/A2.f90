program A2
  use qr_gramschmidt
  implicit none

  real*8, allocatable :: A(:,:), R(:,:), C(:,:)
  real*8, allocatable :: b(:), x(:), y(:)
  integer n

  !  read*, n
  n = 3

  allocate(A(n,n), R(n,n), C(n,n), b(n), x(n), y(n))

  write(*,*) "===================================="
  write(*,*) "            Exercise A.2            "
  write(*,*) "===================================="
  

  call random_number(A)
  C(:,:) = A(:,:)

  call random_number(b)

  write(*,*) "Matrix A="
  call matrix_print(A)

  write(*,*) "Vector b="
  call vector_print(b)

  call qr_decomp(A,R)

  write(*,*) "Matrix Q="
  call matrix_print(A)

  write(*,*) "Matrix R="
  call matrix_print(R)

  y = matmul(transpose(A),b)

  call qr_solve(R,x,y)

  write(*,*) "Vector x="
  call vector_print(x)

  write(*,*) "Check: is Ax = b?"
  call vector_print(matmul(C,x))
  
end program A2

  
