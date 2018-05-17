module functions
  implicit none
  integer, parameter :: n=2
  integer calls

contains

  !System of equations
  function syst_eq(x) result(gs)    
    real*8 x(:), gs(size(x))    
    integer A    
    calls = calls + 1    
    A = 10000    
    gs(1) = A*x(1)*x(2) - 1    
    gs(2) = exp(-x(1)) + exp(-x(2)) - 1d0 - (1d0/A)
  end function syst_eq
  
  function jacobian_syst(x) result(js)
    real*8 x(:), js(size(x),size(x))
    integer A
    calls = calls + 1
    A = 10000
    js(1,1) = A*x(2)
    js(1,2) = A*x(1)
    js(2,1) = -exp(-x(1))
    js(2,2) = -exp(-x(2))
  end function jacobian_syst  

  !Rosenbrock's valley function:
  !f(x,y) = (1-x)**2 + 100*(y-x**2)**2
  function gradient_rosen(x) result(gr) 
    real*8 x(:), gr(size(x))
    calls = calls + 1
    gr(1) = -2*(1-x(1)) - 400*x(1)*(x(2) - x(1)**2) 
    gr(2) = 200*(x(2)-x(1)**2)
  end function gradient_rosen

  function jacobian_rosen(x) result(jr)
    real*8 x(:), jr(size(x),size(x))
    calls = calls + 1
    jr(1,1) = 2 - 400*(x(2)-3*x(1)**2)
    jr(1,2) = -400*x(1)
    jr(2,1) = -400*x(1)
    jr(2,2) = 200d0
  end function jacobian_rosen  

  !Himmelblau's function:
  !f(x,y) = (x**2 + y -11)**2 + (x + y**2 - 7)**2
  function gradient_himmel(x) result(gh)
    real*8 x(:), gh(size(x))
    calls = calls + 1
    gh(1) = 4*x(1)*(x(1)**2 + x(2) - 11) + 2*(x(1) + x(2)**2 - 7)
    gh(2) = 2*(x(1)**2 + x(2) - 11) + 4*x(2)*(x(1) + x(2)**2 - 7)
  end function gradient_himmel

  function jacobian_himmel(x) result(jh)
    real*8 x(:), jh(size(x),size(x))
    calls = calls + 1
    jh(1,1) = 4*(3*x(1)**2 + x(2) - 11) + 2
    jh(1,2) = 4*x(1) + 4*x(2)
    jh(2,1) = 4*x(1) + 4*x(2)
    jh(2,2) = 2 + 4*(x(1) + 3*x(2)**2 - 7)
  end function jacobian_himmel  

end module functions

program exC
  use roots
  use functions
  implicit none
  real*8 x(n), h, eps
  integer steps
  h = 1.5d-8
  eps = 1d-8

  write(*,*) "====================================================================="  
  write(*,*) "                              Exercise C                             "
  write(*,*) "====================================================================="

  write(*,*) "Solving the system of equations:"
  write(*,*) "h=", h, "eps=", eps
  x = [1,-5]  
  write(*,*) "Starting point:", x
  calls = 0
  call newton_rls(syst_eq,x,h,eps,steps)
  write(*,*) "Number of times function was called:", calls
  write(*,*) "Number of steps:", steps  
  write(*,*) "Root=", x
  write(*,*) "Gradient=", syst_eq(x)

  print*
  print*

  write(*,*) "Solving the system of equations:"  
  write(*,*) "h=", h, "eps=", eps
  x = [4,7]  
  write(*,*) "Starting point:", x
  calls = 0
  call newton_rls(syst_eq,x,h,eps,steps)
  write(*,*) "Number of times function was called:", calls
  write(*,*) "Number of steps:", steps  
  write(*,*) "Root=", x
  write(*,*) "Gradient=", syst_eq(x)

  print*
  print*

  write(*,*) "Finding the root of the gradient of the Rosenbrock's valley function:"
  write(*,*) "h=", h, "eps=", eps
  x = [-7,-4]  
  write(*,*) "Starting point:", x
  calls = 0
  call newton_rls(gradient_rosen,x,h,eps,steps)
  write(*,*) "Number of times function was called:", calls
  write(*,*) "Number of steps:", steps
  write(*,*) "Root=", x
  write(*,*) "Gradient=", gradient_rosen(x)

  print*
  print*

  write(*,*) "Finding the root of the gradient of the Rosenbrock's valley function:"  
  write(*,*) "h=", h, "eps=", eps
  x = [2,2]  
  write(*,*) "Starting point:", x
  calls = 0
  call newton_rls(gradient_rosen,x,h,eps,steps)
  write(*,*) "Number of times function was called:", calls
  write(*,*) "Number of steps:", steps
  write(*,*) "Root=", x
  write(*,*) "Gradient=", gradient_rosen(x)

  print*
  print*

  write(*,*) "Finding the root of the gradient of the Himmelblau's function:"
  write(*,*) "h=", h, "eps=", eps
  x = [-1,-1]
  write(*,*) "Starting point:", x
  calls = 0
  call newton_rls(gradient_himmel,x,h,eps,steps)
  write(*,*) "Number of times function was called:", calls
  write(*,*) "Number of steps:", steps    
  write(*,*) "Root=", x
  write(*,*) "Gradient=", gradient_himmel(x)

  print*
  print*

  write(*,*) "Finding the root of the gradient of the Himmelblau's function:"  
  write(*,*) "h=", h, "eps=", eps
  x = [2,1]
  write(*,*) "Starting point:", x
  calls = 0
  call newton_rls(gradient_himmel,x,h,eps,steps)
  write(*,*) "Number of times function was called:", calls
  write(*,*) "Number of steps:", steps    
  write(*,*) "Root=", x
  write(*,*) "Gradient=", gradient_himmel(x)

  print*
  print*

  write(*,*) "Finding the root of the gradient of the Himmelblau's function:"  
  write(*,*) "h=", h, "eps=", eps
  x = [-2,3]
  write(*,*) "Starting point:", x
  calls = 0
  call newton_rls(gradient_himmel,x,h,eps,steps)
  write(*,*) "Number of times function was called:", calls
  write(*,*) "Number of steps:", steps    
  write(*,*) "Root=", x
  write(*,*) "Gradient=", gradient_himmel(x)

  print*
  print*

  write(*,*) "Finding the root of the gradient of the Himmelblau's function:"  
  write(*,*) "h=", h, "eps=", eps
  x = [-5,-6]
  write(*,*) "Starting point:", x
  calls = 0
  call newton_rls(gradient_himmel,x,h,eps,steps)
  write(*,*) "Number of times function was called:", calls
  write(*,*) "Number of steps:", steps    
  write(*,*) "Root=", x
  write(*,*) "Gradient=", gradient_himmel(x)

  print*
  print*

  write(*,*) "Finding the root of the gradient of the Himmelblau's function:"  
  write(*,*) "h=", h, "eps=", eps
  x = [3,-8]
  write(*,*) "Starting point:", x
  calls = 0
  call newton_rls(gradient_himmel,x,h,eps,steps)
  write(*,*) "Number of times function was called:", calls
  write(*,*) "Number of steps:", steps    
  write(*,*) "Root=", x
  write(*,*) "Gradient=", gradient_himmel(x)

end program exC
