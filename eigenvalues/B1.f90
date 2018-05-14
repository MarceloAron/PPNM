program exB1
  use jacobi
  implicit none
  real*8, allocatable :: A(:,:), B(:,:), V(:,:)
  integer i, j, n, stat, sweeps, m
  character(len=32) :: arg

  n = 3

  if (command_argument_count() .gt. 0) then
     call get_command_argument(1,arg)
     read(arg,*,iostat=stat) n
  end if

  if (command_argument_count() .gt. 1) then
     call get_command_argument(2,arg)
     read(arg,*,iostat=stat) m
  else
     m = n
  end if

  allocate(A(n,n),B(n,n),V(n,n))

  if (n .lt. 9) then     
     write(*,*) "===================================="     
     write(*,*) "            Exercise B.1            "     
     write(*,*) "===================================="     
  end if  

  call random_number(A)
  A = A + transpose(A)
  B(:,:) = A(:,:)
  sweeps = evd_low(A,V,m)

   if (n .gt. 9) then
     print*, "n=", n, "sweeps=", sweeps
     return
  end if

  write(*,*) "Eigenvalue decomposition:"
  write (*,*) "Matrix A="
  call matrix_print(B)

  write(*,"(A48,I1,x,A20)") "Matrix A after Jaboci process (finding only the ", m, "lowest eigenvalues):"
  call matrix_print(A)

  write(*,*) "Check: is V^T*A*V = Matrix A after Jacobi process?"
  call matrix_print(matmul(matmul(transpose(V),B),V))

  write(*,*) "Check: V^T * V = 1?"
  call matrix_print(matmul(transpose(V),V))

  write(*,*) "Check: V*V^T = 1?"
  call matrix_print(matmul(V,transpose(V)))

end program exB1
