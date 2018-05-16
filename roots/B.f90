module functions
  implicit none
  integer, parameter :: n=2
  integer calls

contains

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
    jr(2,2) = 200
  end function jacobian_rosen  

  function gradient_himmel(p) result(g)
    real*8 p(:), g(size(p)),x,y
    calls = calls + 1
	x=p(1)
	y=p(2)
! f(x,y)=(x**2+y-11)**2+(x+y**2-7)**2
    g(1) = 4*x*(x**2 + y - 11) + 2*(x + y**2 - 7)
    g(2) = 2*(x**2 + y - 11) + 4*y*(x + y**2 - 7)
  end function gradient_himmel

  function jacobian_himmel(p) result(j)
    real*8 p(:), j(size(p),size(p)),x,y
	x=p(1)
	y=p(2)
    calls = calls + 1
!    j(1,1) = 4*(3*x**2 + y - 11) + 2
	j(1,1)=4*(x**2+y-11)+4*x*(2*x)+2*1
    j(1,2) = 4*x + 4*y
    j(2,1) = 4*x + 4*y
!    j(2,2) = 2 + 4*(x + 3*y**2 - 7)
	j(2,2)=2+4*(x+y**2-7)+4*2*y**2
  end function jacobian_himmel  

end module functions

program exB
  use roots
  use functions
  implicit none
  real*8 x(n), eps, h

  h = 1d-6
  eps = 1d-3

  write(*,*) "====================================================================="  
  write(*,*) "                              Exercise B                             "
  write(*,*) "====================================================================="

  write(*,*) "Finding the root of the gradient of the Rosenbrock's valley function:"
  write(*,*) "eps=", eps
  x = [-2,8]  
  write(*,*) "Starting point:", x
  calls = 0
  call newton_analyt_jac(gradient_rosen,x,h,eps,jacobian_rosen)
  write(*,*) "Number of times function was called:", calls
  write(*,*) "Root=", x
  write(*,*) "Gradient=", gradient_rosen(x)

  print*
  print*
  
  write(*,*) "Finding the root of the gradient of the Himmelblau's function:"
  write(*,*) "eps=", eps
  x = [4,3]
  write(*,*) "Starting point:", x
  calls = 0
  call newton_analyt_jac(gradient_himmel,x,h,eps,jacobian_rosen)
  write(*,*) "Number of times function was called:", calls
  write(*,*) "Root=", x
  write(*,*) "Gradient=", gradient_himmel(x)

end program exB
