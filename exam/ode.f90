module ode
  implicit none

contains

  !RK4 using Runge's principle, in which the error is proportional to the
  !difference between the full step integration and two half steps integration
  function rk4(t,y,f,h,err) result(yh)    
    real*8 t, h, y(:), yh(size(y)), err(size(y))    
    interface       
       function f(t,y) result(df)         
         real*8 t, y(:), df(size(y))         
       end function f
    end interface
    real*8 k0(size(y)), k1(size(y)), k2(size(y)), k3(size(y)), k(size(y)), yhalf(size(y))
    integer p

    p = 4

    k0 = f(t,y)
    k1 = f(t+h/2,y+k0*h/2)
    k2 = f(t+h/2,y+k1*h/2)
    k3 = f(t+h,y+k2*h)
    k = (k0 + 2*k1 + 2*k2 + k3)/6
    yh = y + k*h !Full step

    k0 = f(t,y)
    k1 = f(t+h/4,y+k0*h/4)
    k2 = f(t+h/4,y+k1*h/4)
    k3 = f(t+h/2,y+k2*h/2)
    k = (k0 + 2*k1 + 2*k2 + k3)/12
    yhalf = y + 2*k*h !Two half steps

    err = abs(yh-yhalf)/(2**p - 1) !Step-doubling method
  end function rk4

  
  function driver(f,a,y,b,acc,eps) result(path)
    real*8 a, b, eps, acc, y(:)
    interface
       function f(t,y) result(df)
         real*8 t, y(:), df(size(y))
       end function f
    end interface
    real*8, allocatable :: path(:,:), tmp(:,:), yh(:)
    real*8 t, h, tol(size(y)), err(size(y))
    integer k, m, n

    m = size(y)
    n = 1
    k = 100 
    allocate(path(k,m+1)) !path is a matrix that has all the values of t and y(t)
    t = a
    h = (b-a)/20d0
    path(n,1) = t !First column of path is t
    path(n,2:) = y !Second and third column of path is y(t)
    
    do while (n .le. k)
       if (t .ge. b) then !b is the end of our interval, so we exit
          exit !THIS IS WHAT STOPS THE PROGRAM
       end if
       if (t+h .gt. b) then
          h = b - t         !Change h so we arrive exactly at b
       end if

       yh = rk4(t,y,f,h,err) !Now we do a step
       tol = (acc+norm2(yh)*eps)*sqrt(h/(b-a)) !Calculating the tolerance

       if (maxval(err/tol) .lt. 1) then !Acceptance criteria
!          write(0,*) "Accepted" !Test to see if the criteria was being accepted
          n = n + 1  
          t = t + h  !Take a step 
          y = yh     !That's the result from the stepper
          if (n .eq. k) then !We have to keep it going until we reach b
             tmp = path !Defining a temporary matrix to store path
             deallocate(path)
             k = 2*k !New k
             allocate(path(k,m+1)) !Raise the number of columns of path
             path(1:n,:) = tmp(:,:) !Return the old values to path
          end if
          path(n,1) = t  !Assigning the new values to path
          path(n,2:) = y
       end if

       if (minval(abs(err)) .eq. 0) then
          h = h/2 !Arbitrary h just because err is zero
       else
          h = 0.95d0*h*minval(tol/err)**0.25d0 !Empirical expression to find new h
!          write(0,*) "Empirical" !Test to see if the program actually used this
       end if
    end do
    tmp = path
    deallocate(path)
    allocate(path(n,m+1)) !This is just so path doesn't have unnecessary columns
    path(:,:) = tmp(1:n,:)
  end function driver

end module ode

       
         
