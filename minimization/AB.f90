module functions
  implicit none
  integer, parameter :: n=2

contains

  function rosen(x) result(r)
    real*8 x(:), r
    r = (1-x(1))**2 + 100*(x(2) - x(1)**2)**2
  end function rosen

  function gradient_rosen(x) result(gr)
    real*8 x(:), gr(size(x))
    gr(1) = -2*(1-x(1)) -400*x(1)*(x(2) - x(1)**2)
    gr(2) = 200*(x(2) - x(1)**2)
  end function gradient_rosen

  function hessian_rosen(x) result(hr)
    real*8 x(:), hr(size(x),size(x))
    hr(1,1) = 2 - 400*(x(2) - 3*x(1)**2)
    hr(1,2) = -400*x(1)
    hr(2,1) = -400*x(1)
    hr(2,2) = 200
  end function hessian_rosen  
  
  function himmel(x) result(h)
    real*8 x(:), h
    h = (x(1)**2 + x(2) - 11)**2 + (x(1) + x(2)**2 - 7)**2
  end function himmel

  function gradient_himmel(x) result(gh)
    real*8 x(:), gh(size(x))
    gh(1) = 4*x(1)*(x(1)**2 + x(2) - 11) + 2*(x(1) + x(2)**2 - 7)
    gh(2) = 2*(x(1)**2 + x(2) - 11) + 4*x(2)*(x(1) + x(2)**2 - 7)
  end function gradient_himmel

  function hessian_himmel(x) result(hh)
    real*8 x(:), hh(size(x),size(x))
    hh(1,1) = 4*(3*x(1)**2 + x(2) - 11) + 2
    hh(1,2) = 4*x(1) + 4*x(2)
    hh(2,1) = 4*x(1) + 4*x(2)
    hh(2,2) = 2 + 4*(x(1) + 3*x(2)**2 - 7)
  end function hessian_himmel

  function test(x) result(t)
    real*8 x(:), t
    t = (x(1)-5)**2 +(x(2)-3)**2
  end function test

  function gradient_test(x) result(gt)
    real*8 x(:), gt(size(x))
    gt(1) = 2*x(1) - 10
    gt(2) = 2*x(2) - 6
  end function gradient_test

  function hessian_test(x) result(ht)
    real*8 x(:), ht(size(x),size(x))
    ht(1,1) = 2
    ht(1,2) = 0
    ht(2,1) = 0
    ht(2,2) = 2
  end function hessian_test  

end module functions

program exA
  use minimization
  use functions
  implicit none
  real*8, allocatable :: simplex(:,:)
  real*8 x(n), eps, simplex_size_goal
  integer steps, d

  eps = 1.5d-8

  write(*,*) "====================================================================="  
  write(*,*) "                              Exercise A                             "
  write(*,*) "====================================================================="

  write(*,*) "Minimum of the Rosenbrock's valley function:"
  x = [-2,8]
  write(*,*) "Starting point:", real(x)
  call newton_minim(rosen,x,gradient_rosen,hessian_rosen,eps,steps)
  write(*,*) "Number of steps:", steps
  write(*,*) "Minimum at:", real(x)
  write(*,*) "Function at this point:", real(rosen(x))
  write(*,*) "Gradient at this point:", real(gradient_rosen(x))    

  print*
  print*

  write(*,*) "Minimum of the Himmelblau's function:"
  x = [-1,5]
  write(*,*) "Starting point:", real(x)
  call newton_minim(himmel,x,gradient_himmel,hessian_himmel,eps,steps)
  write(*,*) "Number of steps:", steps
  write(*,*) "Minimum at:", real(x)
  write(*,*) "Function at this point:", real(himmel(x))
  write(*,*) "Gradient at this point:", real(gradient_himmel(x))

  print*
  print*
  
  write(*,*) "Minimum of the test function: f(x,y) = (x-y+5)**2"
  x = [1,-1]
  write(*,*) "Starting point:", real(x)
  call newton_minim(test,x,gradient_test,hessian_test,eps,steps)
  write(*,*) "Number of steps:", steps
  write(*,*) "Minimum at:", real(x)
  write(*,*) "Function at this point:", real(test(x))
  write(*,*) "Gradient at this point:", real(gradient_test(x))

  write(*,*) "====================================================================="  
  write(*,*) "                              Exercise B                             "
  write(*,*) "====================================================================="

  write(*,*) "Minimum of the Rosenbrock's valley function:"
  x = [3,-4]
  write(*,*) "Starting point:", real(x)
  call quasi_newton(rosen,x,eps,steps)
  write(*,*) "Number of steps:", steps
  write(*,*) "Minimum at:", real(x)
  write(*,*) "Function at this point:", real(rosen(x))
  write(*,*) "Numerical gradient at this point:", real(numgrad(rosen,x))
  write(*,*) "Analytical gradient at this point:", real(gradient_rosen(x))

  print*
  print*

  write(*,*) "Minimum of the Himmelblau's function:"
  x = [1,1]
  write(*,*) "Starting point:", real(x)
  call quasi_newton(himmel,x,eps,steps)
  write(*,*) "Number of steps:", steps
  write(*,*) "Minimum at:", real(x)
  write(*,*) "Function at this point:", real(himmel(x))
  write(*,*) "Numerical gradient at this point:", real(numgrad(himmel,x))
  write(*,*) "Analytical gradient at this point:", real(gradient_himmel(x))

  print*
  print*
  
  write(*,*) "Minimum of the test function: f(x,y) = (x-5)**2 + (y-3)**2"
  x = [-87,32]
  write(*,*) "Starting point:", real(x)
  call quasi_newton(test,x,eps,steps)
  write(*,*) "Number of steps:", steps
  write(*,*) "Minimum at:", real(x)
  write(*,*) "Function at this point:", real(test(x))
  write(*,*) "Numerical gradient at this point:", real(numgrad(test,x))
  write(*,*) "Analytical gradient at this point:", real(gradient_test(x))

!  write(*,*) "====================================================================="  
!  write(*,*) "                              Exercise C                             "
!  write(*,*) "====================================================================="

!  d = 2
!  simplex_size_goal = 1d-10
!  allocate(simplex(d+1,d))
!  simplex(1,1) = -3
!  simplex(1,2) = -3
!  simplex(2,1) = -5
!  simplex(2,2) = -5
!  simplex(3,1) = -2
!  simplex(3,2) = -2

!  steps = downhill_simplex(himmel,simplex,d,simplex_size_goal)

!  write(*,*) "Number of iterations:", steps
  

end program exA



  

    
    
