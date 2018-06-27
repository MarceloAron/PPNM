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
  acc = 1d-4
  eps = 1d-4
  write(0,"(x,A19,x,F6.4)") "Absolute precision:", acc
  write(0,"(x,A19,x,F6.4)") "Relative precision:", real(eps)
  y = [0,1]
  write(0,*) "ODE: u'' = -u"  
  write(0,"(x,A19,2x,A6,x,F3.1,A1,3x,A7,x,F3.1)") "Initial conditions:", "y(a) =", y(1),",", "y'(a) =", y(2)
  path = driver(sineq,a,y,b,acc,eps)

  do i=1,size(path,1)
     write(*,*) (real(path(i,j)), j=1,size(path,2))
  end do

  write(0,"(x,A8,4x,A6,x,F8.6,4x,A7,x,F8.6)") "Results:", "y(b) =", y(1), "y'(b) =", y(2) 
  write(0,"(x,A9,x,A8,x,F8.6,3x,A8,x,F8.6)") "Should be:", "sin(b) =", sin(b), "cos(b) =", cos(b)
  write(0,"(x,A7,12x,F8.6,12x,F8.6)") "Errors:", abs(sin(b)-y(1)), abs(cos(b)-y(2))
  
  write(0,*)
  write(0,*)
  

  write(0,*) "======================================================================"
  write(0,*) "                              Exercise C                              "
  write(0,*) "======================================================================"

  write(0,*) "Function = sin(x)"  
  a = 0
  b = 3.14159265d0
  y = [0]
  write(0,"(x,A9,2x,F3.1,x,A2,x,F10.8)") "Interval:", a, "to", b
  path = driver(integ_one,a,y,b,acc,eps)
  write(0,"(x,A10,x,F8.6)") "Integral =", y(1)
  write(0,"(x,A10,x,F8.6)") "Should be:", 2d0

  write(0,*)
  write(0,*) "----------------------------------------------------------------------"
  write(0,*)

  write(0,*) "Function = x*exp(-x**2)"
  a = 0
  b = 5
  y = [0]
  write(0,"(x,A9,2x,F3.1,x,A2,x,F3.1)") "Interval:", a, "to", b  
  path = driver(integ_two,a,y,b,acc,eps)
  write(0,"(x,A10,x,F8.6)") "Integral =", y(1)
  write(0,"(x,A10,x,F8.6)") "Should be:", 0.5d0

  write(0,*)
  write(0,*) "----------------------------------------------------------------------"
  write(0,*)

  write(0,*) "Function = 1/sqrt(x)"
  a = 1d-8
  b = 1
  y = [0]
  write(0,"(x,A9,2x,F10.8,x,A2,x,F3.1)") "Interval:", a, "to", b  
  path = driver(integ_three,a,y,b,acc,eps)
  write(0,"(x,A10,x,F8.6)") "Integral =", y(1)
  write(0,"(x,A10,x,F8.6)") "Should be:", 1.9998d0

end program main
