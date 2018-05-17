module roots
  use qr_givens
  implicit none

contains

!Numerical Jacobian done by Dmitri Fedorov to check if the analytical was right  
  function numjac(f,p) result (jac)    
    interface       
       function f(p) result (fp)         
         real*8 p(:),fp(size(p))         
       end function f       
    end interface    
    real*8 p(:),jac(size(p),size(p)),pi,h,fp(size(p)),df(size(p))    
    real*8,parameter:: dx=1.d0/67108864    
    integer i,j    
    fp=f(p)    
    do i=1,size(p)       
       pi=p(i)       
       h=abs(pi)*dx       
       p(i)=pi+h       
       df=f(p)-fp       
       do j=1,size(p)          
          jac(j,i)=df(j)/h          
       end do       
       p(i)=pi       
    end do    
  end function numjac 

  subroutine newton(f,x,h,eps,steps)
    real*8 x(:), h, eps, lambda
    interface
       function f(x)
         real*8 x(:), f(size(x))
       end function f
    end interface
    real*8, allocatable :: fx(:), df(:), jac(:,:), dx(:)
    integer i, j, n, steps

    n = size(x)
    steps = 0

    allocate(jac(n,n))

    do
       fx = f(x) !This is f(x1,...,xj,...,xk)
       do j=1,n
          x(j) = x(j) + h !Now f(x) is f(x1,...,xj+h,...,xk)
          df = f(x) - fx 
          do i=1,n
             jac(i,j) = df(i)/h !Calculate the approximate numerical derivative
          end do
          x(j) = x(j) - h !Set xj back to normal to do it again for a different j
       end do
       call qr_givens_decomp(jac)
       dx = qr_givens_solve(jac,-fx) !Solve Jdx=-f(x) for dx
       lambda = 1d0
       do while (norm2(f(x+lambda*dx)) .gt. (1d0-lambda/2d0)*norm2(fx) .and. lambda .gt. 1d0/64d0)
          lambda = lambda/2d0 !This is the simple backtracking linesearch
       end do
       x = x + lambda*dx
       steps = steps + 1
       if (norm2(dx) .lt. h .or. norm2(f(x)) .lt. eps) then
          exit
       end if
    end do
  end subroutine newton

  subroutine newton_analyt_jac(f,x,eps,g,steps)
    real*8 x(:), eps, lambda
    real*8, allocatable :: dx(:), jac(:,:), jc(:,:), fx(:), z(:)
    interface
       function f(x)
         real*8 x(:), f(size(x))
       end function f
       function g(x)
         real*8 x(:), g(size(x),size(x))
       end function g
    end interface
    integer i, j, n, steps

    n = size(x)
    steps = 0

    do       
       jac = g(x)
       fx = f(x)
       call qr_givens_decomp(jac)       
       dx = qr_givens_solve(jac,-fx)
       lambda = 1d0
       do while (norm2(f(x+lambda*dx)) .gt. (1d0-lambda/2d0)*norm2(fx) .and. lambda .gt. 1d0/64d0)
          lambda = lambda/2d0
       end do
       x = x + lambda*dx
       steps = steps + 1
       if (norm2(f(x)) .lt. eps) then
          exit
       end if
    end do
  end subroutine newton_analyt_jac

  subroutine newton_rls(f,x,eps,h,steps)
    real*8 x(:), h, eps, lambda, g0, gp0, glambda, c
    interface       
       function f(x)
         real*8 x(:), f(size(x))
       end function f
    end interface
    real*8, allocatable :: fx(:), df(:), jac(:,:), dx(:), fxdx(:)
    integer i, j, n, steps

    n = size(x)
    steps = 0

    allocate(jac(n,n))

    do
       fx = f(x)
       do j=1,n
          x(j) = x(j) + h
          df = f(x) - fx 
          do i=1,n
             jac(i,j) = df(i)/h
          end do
          x(j) = x(j) - h 
       end do
       call qr_givens_decomp(jac)
       dx = qr_givens_solve(jac,-fx)
       lambda = 1d0
       g0 = 0.5d0*dot_product(fx,fx)
       gp0 = -dot_product(fx,fx)       
       do while (norm2(f(x+lambda*dx)) .gt. (1d0-lambda/2d0)*norm2(fx))! .and. lambda .gt. 1d0/64d0) !Defining the minimum lambda this way for
          glambda = 0.5d0*dot_product(f(x+lambda*dx),f(x+lambda*dx))                               !the refined linesearch wasn't effective
          c = (glambda - g0 - gp0*lambda)/(lambda*lambda)
          lambda = -gp0/(2d0*c)
          if (lambda .lt. 1d0/64d0) then !Restriction so that lambda is not really small
             lambda = 1d0/64d0           
             exit
          end if          
       end do
       x = x + lambda*dx
       steps = steps + 1
       if (norm2(dx) .lt. h .or. norm2(f(x)) .lt. eps) then
          exit
       end if
    end do
  end subroutine newton_rls

  subroutine newton_analyt_jac_rls(f,x,eps,g,steps)
    real*8 x(:), eps, lambda
    real*8, allocatable :: dx(:), jac(:,:), fx(:), z(:), g0, gp0, glambda, c
    interface
       function f(x)
         real*8 x(:), f(size(x))
       end function f
       function g(x)
         real*8 x(:), g(size(x),size(x))
       end function g
    end interface
    integer i, j, n, steps

    n = size(x)
    steps = 0

    do       
       jac = g(x)
       fx = f(x)
       call qr_givens_decomp(jac)       
       dx = qr_givens_solve(jac,-fx)
       lambda = 1d0
       g0 = 0.5d0*dot_product(fx,fx)
       gp0 = -dot_product(fx,fx)
       do while (norm2(f(x+lambda*dx)) .gt. (1d0-lambda/2d0)*norm2(fx))
           glambda = 0.5d0*dot_product(f(x+lambda*dx),f(x+lambda*dx))
          c = (glambda - g0 - gp0*lambda)/(lambda*lambda)
          lambda = -gp0/(2d0*c)
          if (lambda .lt. 1d0/64d0) then 
             lambda = 1d0/64d0           
             exit
          end if          
       end do
       x = x + lambda*dx
       steps = steps + 1
       if (norm2(f(x)) .lt. eps) then
          exit
       end if
    end do
  end subroutine newton_analyt_jac_rls  

end module roots

