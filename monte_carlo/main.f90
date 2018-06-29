module functions
  implicit none

contains

  function integ_one(x) result(f)
    real*8 x(:), f
    f = x(1)*x(1)
  end function integ_one

  function integ_two(x) result(f)
    real*8 x(:), f
    f = x(1)**2 + x(2)**2 + x(3)**2
  end function integ_two

  function integ_three(x) result(f)
    real*8 x(:), f, pi
    pi = 3.14159265359d0
    f = (1-cos(x(1))*cos(x(2))*cos(x(3)))**(-1)/pi**3
  end function integ_three

  function c_one(x)
    real*8 x, c_one
    c_one = x
  end function c_one

  function d_one(x)
    real*8 x, d_one
    d_one = x**3
  end function d_one

  function integ_2D_one(x,y) result(f)
    real*8 x, y, f
    f = x**2 + y**2
  end function integ_2D_one  

end module functions

program main
  use montecarlo
  use functions
  use integration
  implicit none
  real*8, allocatable :: a(:), b(:)
  real*8 res, err, exact, pi, acc, eps, aC, bC
  integer N, dim, i

  pi = 3.14159265359d0

  write(*,*) "======================================================================"
  write(*,*) "                              Exercise A                              "
  write(*,*) "======================================================================"

  write(*,*) "Integrating r^2 dr dphi dtheta from 0-1, 0-2pi, 0-pi"
  N = 1e6
  !Integration limits are 0-1, 0-2pi, 0-pi
  a = [0,0,0]
  b = [1d0,2*pi,pi]
  dim = size(a)
  call plain_montecarlo(integ_one,a,b,dim,N,res,err)
  exact = 6.579736267393d0
  write(*,"(x,A17,2x,I7)") "Number of points:", N  
  write(*,"(x,A10,x,F13.11)") "Integral =", res
  write(*,"(x,A10,x,F13.11)") "Should be:", exact
  write(*,"(x,A16,x,F13.11)") "Estimated error:", err
  write(*,"(x,A13,4x,F13.11)") "Actual error:", abs(res-exact)

  write(*,*)
  write(*,*) "----------------------------------------------------------------------"
  write(*,*)

  write(*,*) "Integrating x**2 + y**2 + z**2 dx dy dz from 0-1, 0-2, 0-3"  
  N = 1e6
  !Integration limits are 0-1, 0-2, 0-3
  a = [0,0,0]
  b = [1,2,3]
  dim = size(a)
  call plain_montecarlo(integ_two,a,b,dim,N,res,err)
  exact = 28d0
  write(*,"(x,A17,2x,I7)") "Number of points:", N  
  write(*,"(x,A10,x,F10.5)") "Integral =", res
  write(*,"(x,A10,x,F10.5)") "Should be:", exact
  write(*,"(x,A16,x,F10.5)") "Estimated error:", err
  write(*,"(x,A13,4x,F10.5)") "Actual error:", abs(res-exact)

  write(*,*)  
  write(*,*) "----------------------------------------------------------------------"
  write(*,*)

  write(*,*) "Integrating (1-cos(x)*cos(y)*cos(z))**(-1)/pi**3 dx dy dz from 0-pi, 0-pi, 0-pi"  
  N = 1e6
  !Integration limits are 0-1, 1-2, 2-3, 3-4
  a = [0,0,0]
  b = [pi,pi,pi]
  dim = size(a)
  call plain_montecarlo(integ_three,a,b,dim,N,res,err)
  exact = 1.393203929686d0
  write(*,"(x,A17,2x,I7)") "Number of points:", N  
  write(*,"(x,A10,x,F14.12)") "Integral =", res
  write(*,"(x,A10,x,F14.12)") "Should be:", exact
  write(*,"(x,A16,x,F14.12)") "Estimated error:", err
  write(*,"(x,A13,4x,F14.12)") "Actual error:", abs(res-exact)

  write(*,*) "======================================================================"  
  write(*,*) "                              Exercise B                              "
  write(*,*) "======================================================================"

  N = 1000
  a = [0,0,0]
  b = [1d0,2*pi,pi]
  dim = size(a)
  !Now we will call the Monte Carlo subroutine multiple times, each time
  !with a different number of points, to show how the error varies.
  do i=10,N,10
     call plain_montecarlo(integ_one,a,b,dim,i,res,err)
     write(0,*) i, err
  end do
  write(*,*) "Results shown in the form of a plot"

!  write(*,*) "======================================================================"  
!  write(*,*) "                              Exercise C                              "
!  write(*,*) "======================================================================"

!  acc = 1d-7
!  eps = 1d-7
!  aC = 0
!  bC = 2
!  exact = 39.4667d0
!  write(*,"(x,A19,x,F9.7)") "Absolute precision:", acc
!  write(*,"(x,A19,x,F9.7)") "Relative precision:", eps
!  write(*,*) "Integrating x**2 + y**2 from 0-1, x-x**3"
!  res = adapt2D(integ_2D_one,c_one,d_one,aC,bC,acc,eps,err)
!  write(*,*) "Integral =", res
!  write(*,*) "Should be:", exact
!  write(*,"(x,A16,x,F14.12)") "Estimated error:", err
!  write(*,"(x,A13,4x,F14.12)") "Actual error:", abs(res-exact)
  

end program main

  
    
