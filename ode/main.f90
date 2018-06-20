module functions
  implicit none

contains

  function sineq(t,y) result(dydt)
    real*8 t, y(:), dydt(size(y))
    dydt(1) = y(2)
    dydt(2) = -y(1)
  end function sineq

  function integ_one(t,y) result(dydt)
    real*8 t, y(:), dydt(size(y))
    dydt(1) = sin(t)
  end function integ_one

  function integ_two(t,y) result(dydt)
    real*8 t, y(:), dydt(size(y))
    dydt(1) = t*exp(-t**2)
  end function integ_two

  function integ_three(t,y) result(dydt)
    real*8 t, y(:), dydt(size(y))
    dydt(1) = 1/sqrt(t)
  end function integ_three  

end module functions

program main
  use ode
  use functions
  implicit none
  real*8, allocatable :: path(:,:), y(:)
  real*8 a, b, acc, eps
  integer i, j

  write(0,*) "====================================================================="
  write(0,*) "                          Exercises A and B                          "
  write(0,*) "====================================================================="

  a = 0
  b = 7
  acc = 1d-2
  eps = 1d-2
!  allocate(y(2))
  y = [0,1]
  write(0,*) "ODE: u'' = -u"  
  write(0,*) "Initial conditions:", "   y(a)=", real(y(1)), "y'(a)=", real(y(2))
  write(0,*) "Integration errors estimates"  
  path = driver(sineq,a,y,b,acc,eps)

  do i=1,size(path,1)
     write(*,*) (real(path(i,j)), j=1,size(path,2))
  end do

  write(0,*)  
  write(0,*) "y(b)=", y(1), "y'(b)=", y(2) 
  write(0,*) "Should be:", "sin(b)=", sin(b), "cos(b)=", cos(b)
  write(0,*) "Errors:", real(abs(sin(b)-y(1))), real(abs(cos(b)-y(2)))
  
  write(0,*)
  write(0,*)
  

  write(0,*) "======================================================================"
  write(0,*) "                              Exercise C                              "
  write(0,*) "======================================================================"

  write(0,*) "Function = sin(x)"  
  a = 0
  b = 3.141592
  y = [0]
  write(0,*) "Interval:", real(a), "to", real(b)
  path = driver(integ_one,a,y,b,acc,eps)
  write(0,*) "Integral=", real(y(1))
  write(0,*) "Should be=", real(2)

  write(0,*)
  write(0,*) "----------------------------------------------------------------------"
  write(0,*)

  write(0,*) "Function = x*exp(-x**2)"
  a = 0
  b = 5
  y = [0]
  write(0,*) "Interval:", real(a), "to", real(b)
  path = driver(integ_two,a,y,b,acc,eps)
  write(0,*) "Integral=", real(y(1))
  write(0,*) "Should be=", real(0.5)

  write(0,*)
  write(0,*) "----------------------------------------------------------------------"
  write(0,*)

  write(0,*) "Function = 1/sqrt(x)"
  a = 1d-8
  b = 1
  y = [0]
  write(0,*) "Interval:", a, "to", real(b)
  path = driver(integ_three,a,y,b,acc,eps)
  write(0,*) "Integral=", real(y(1))
  write(0,*) "Should be=", real(1.9998)

end program main
