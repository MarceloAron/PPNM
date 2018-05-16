module roots
  use qr_givens
  implicit none

contains

  subroutine newton(f,x,h,eps)
    real*8 x(:), h, eps, lambda
    interface
       function f(x)
         real*8 x(:), f(size(x))
       end function f
    end interface
    real*8, allocatable :: fx(:), df(:), jac(:,:), dx(:), jc(:,:)
    integer i, j, n

    n = size(x)

    allocate(jac(n,n),jc(n,n))

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
       jc = jac
       call qr_givens_decomp(jac)
       dx = qr_givens_solve(jac,-fx) !Solve Jdx=-f(x) for dx
       lambda = 1d0
       do while (norm2(f(x+lambda*dx)) .gt. (1-lambda/2)*norm2(fx) .and. lambda .gt. 1d0/64d0)
          lambda = lambda/2          
       end do              
       x = x + lambda*dx
       if (norm2(dx) .lt. h .or. norm2(f(x)) .lt. eps) then
          exit
       end if
    end do
  end subroutine newton

  subroutine newton_analyt_jac(f,x,eps,g)
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
    integer i, j, n

    n = size(x)

    do       
       jac = g(x)
       fx = f(x)
       call qr_givens_decomp(jac)       
       dx = qr_givens_solve(jac,-fx)
       lambda = 1
       do while (norm2(f(x+lambda*dx)) .gt. (1-lambda/2)*norm2(fx) .and. lambda .gt. 1d0/64d0)
          lambda = lambda/2
       end do
       x = x + lambda*dx
       if (norm2(f(x)) .lt. eps) then
          exit
       end if
    end do
  end subroutine newton_analyt_jac        

end module roots

