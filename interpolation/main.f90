program main
  use linspline
  use quadspline
  use cubespline
  implicit none

  real*8, allocatable :: x(:), y(:)
  real*8 z, r
  integer i,n
  type (qspline) :: q
  type (cspline) :: cu

  n = 20
  allocate(x(n),y(n))

  do i=1,n
     call random_number(r)     
     x(i) = i + 0.5 
     y(i) = 0.2*i + r
     print*, x(i), y(i)
  end do

  print*
  print*

  q = qspline_alloc(x,y)
  cu = cspline_alloc(x,y)
  z = x(1)

  do while (z .le. x(n))
     print*, z, lspline(x,y,z), qspline_eval(q,z), cspline_eval(cu,z), lint(x,y,z), qspline_int(q,z), cspline_int(cu,z), &
& qspline_der(q,z), cspline_der(cu,z)
     z = z + 0.1
  end do

end program main

  
