module minimization
  use qr_givens
  implicit none

  type neuron
     integer n
!     interface
!        function f(x)
!          real*8 x, f
!        end function f
!     end interface
     real*8, allocatable :: data(:)
!     real*8 f
  end type neuron  

contains

  subroutine newton_minim(f,x,g,h,eps,steps)
    real*8 x(:), eps, lambda, func
    real*8, allocatable :: dx(:), grad(:), hess(:,:)
    interface
       function f(x)
         real*8 x(:), f
       end function f
       function g(x)
         real*8 x(:), g(size(x))
       end function g
       function h(x)
         real*8 x(:), h(size(x),size(x))
       end function h       
    end interface
    real*8, parameter :: a=1d-4
    integer i, j, steps

    steps = 0

    do
       func = f(x)
       grad = g(x)
       hess = h(x)
       call qr_givens_decomp(hess)
       dx = qr_givens_solve(hess,-grad)
       lambda = 1d0
       do while (f(x+lambda*dx) .gt. (func + a*lambda*dot_product(dx,grad)) .and. lambda .gt. 1d0/64d0)            
          lambda = lambda/2
       end do
       x = x + lambda*dx
       steps = steps + 1
       if (norm2(g(x)) .lt. eps) then
          exit
       end if
    end do
  end subroutine newton_minim

  function numgrad(f,x,network,xlist,ylist,func) result(grad)
    interface
       function f(x,network,xlist,ylist,func)
         import neuron         
         interface
            function func(t)
              real*8 t, func
            end function func
         end interface         
!         import neuron
         real*8 x(:), f, xlist(:), ylist(:)
         type (neuron) :: network
       end function f
       function func(t)
         real*8 t, func
       end function func       
    end interface
    real*8 x(:), grad(size(x)), fx, xlist(:), ylist(:)
    real*8, parameter :: h = 1.5d-8
    integer i
    type (neuron) :: network

    fx = f(x,network,xlist,ylist,func)
    do i=1,size(x)
       x(i) = x(i) + h
       grad(i) = (f(x,network,xlist,ylist,func) - fx)/h
       x(i) = x(i) - h
    end do
  end function numgrad

  subroutine identity_matrix(A)
    real*8 A(:,:)
    integer i
    A = 0
    do i=1,size(A,1)
       A(i,i) = 1
    end do
  end subroutine identity_matrix

  subroutine broyden_sym_update(A,beta,u,v)
    real*8 u(:), v(:), beta, A(size(u),size(v))
    integer i, j

    do i=1,size(u)
       do j=1,size(v)
          A(i,j) = A(i,j) + beta*u(i)*v(j)
       end do
    end do
  end subroutine broyden_sym_update  
    
  subroutine quasi_newton(f,x,eps,steps,network,xlist,ylist,func)
    real*8 x(:), eps, lambda, xlist(:), ylist(:)
    real*8, parameter :: dx=1.5d-5, alpha=1d-2
    interface
       function f(x,network,xlist,ylist,func)
         import neuron
         interface
            function func(t)
              real*8 t, func
            end function func
         end interface         
!         import neuron
         real*8 x(:), f, xlist(:), ylist(:)
         type (neuron) :: network
       end function f
       function func(t)
         real*8 t, func
       end function func       
    end interface
    real*8 hess_inv(size(x),size(x)), s(size(x)), u(size(x)), y(size(x)), a(size(x))
    real*8 gx(size(x)), z(size(x)), gz(size(x))
    real*8 fx, fz, uy, sy, gamma
    integer i, j, steps
    type (neuron) :: network

    steps = 0
    call identity_matrix(hess_inv) !Start by setting the inverse Hessian as an identity matrix
    gx = numgrad(f,x,network,xlist,ylist,func) !So we don't have to call the functions multiple times inside the loop
    fx = f(x,network,xlist,ylist,func)      !Same as above

    do
       steps = steps + 1       
       s = -matmul(hess_inv,gx)
        if (norm2(s) .lt. dx) then
          write(*,*) "The method converged: |s| < dx"
          write(*,*) real(norm2(s)), "<", real(dx)
          exit
       end if       
       if (norm2(gx) .lt. eps) then
          write(*,*) "The method converged: |Gradient(x)| < eps"
          write(*,*) real(norm2(gx)), "<", real(eps)
          exit
       end if
       lambda = 1d0
!       write(*,*) "norm2(s)=", real(norm2(s)), "dx=", real(dx), "norm2(g)x=", real(norm2(gx)), "eps=", real(eps)
!       do while (f(x+lambda*s) .gt. (fx + alpha*dot_product(s,numgrad(f,x))) .and. lambda .gt. 1d0/64d0)
!          lambda = lambda/2
!          if (lambda .lt. 1d0/64d0) then             
!             call identity_matrix(hess_inv)
!             exit
!          end if          
!       end do
       do
          z = x + lambda*s
          fz = f(z,network,xlist,ylist,func)
          if (fz .lt. (fx + alpha*dot_product(s,gx))) then
             exit
          end if
          lambda = lambda/2
          if (lambda .lt. 1d0/64d0) then
!             call identity_matrix(hess_inv)
             exit
          end if
       end do
       gz = numgrad(f,z,network,xlist,ylist,func)
       y = gz - gx
       u = s - matmul(hess_inv,y)
       uy = dot_product(u,y)
       sy = dot_product(s,y)
       if (sy .gt. eps) then
          gamma = uy/(2*sy)
          a = (u - gamma*s) 
          call broyden_sym_update(hess_inv,1/sy,a,s);
          call broyden_sym_update(hess_inv,1/sy,s,a);
       end if
       x = z
       fx = fz
       gx = gz
    end do    
  end subroutine quasi_newton

  subroutine reflection(highest,centroid,dim,reflected)
    real*8 highest(:), centroid(:), reflected(:)
    integer i, dim
    do i=1,dim
       reflected(i) = 2*centroid(i) - highest(i)
    end do
  end subroutine reflection

  subroutine expansion(highest,centroid,dim,expanded)
    real*8 highest(:), centroid(:), expanded(:)
    integer i, dim
    do i=1,dim
       expanded(i) = 3*centroid(i) - 2*highest(i)
    end do
  end subroutine expansion

  subroutine contraction(highest,centroid,dim,contracted)
    real*8 highest(:), centroid(:), contracted(:)
    integer i, dim
    do i=1,dim
       contracted(i) = 0.5*(highest(i) + centroid(i))
    end do
  end subroutine contraction

  subroutine reduction(simplex,dim,low)
    real*8 simplex(:,:)
    integer i, k, dim, low
    do k=1,dim+1
       if (k .ne. low) then          
          do i=1,dim
             simplex(k,i) = 0.5*(simplex(k,i) + simplex(low,i))
          end do
       end if
    end do
  end subroutine reduction  

  function distance(a,b,dim) result(s)
    real*8 a(:), b(:), s
    integer i,dim
    s = 0
    do i=1,dim
       s = s + sqrt((b(i) - a(i))**2)
    end do
  end function distance

  function simplex_size(simplex,dim) result(s)
    real*8 simplex(:,:), s, dist
    integer i, k, dim
    s = 0
    do i=2,dim+1
       dist = distance(simplex(1,:),simplex(k,:),dim)
       if (dist .gt. s) then
          s = dist
       end if
    end do
  end function simplex_size

  subroutine simplex_update(simplex,f_values,d,high,low,centroid)
    real*8 simplex(:,:), f_values(:), centroid(:), highest, lowest, next, s
    integer i, k, dim, high, low, d
    high = 0
    low = 0    
    highest = f_values(1)
    lowest = f_values(1)
    do k=2,dim+1
       next = f_values(k)
       if (next .gt. highest) then
          highest = next
          high = k
       end if
       if (next .lt. lowest) then
          lowest = next
          low = k
       end if
    end do
    do i=1,d
       s = 0
       do k=1,d+1
          if (k .ne. high) then
             s = s + simplex(k,i) 
          end if
       end do
       centroid(i) = s/d       
    end do
  end subroutine simplex_update

  subroutine simplex_initiate(f,simplex,f_values,d,high,low,centroid)
    interface
       function f(x)
         real*8 x(:), f
       end function f
    end interface
    real*8 simplex(:,:), f_values(:), centroid(:)
    integer i, k, d, high, low
    do k=1,d+1
       f_values(k) = f(simplex(k,:))
    end do
    call simplex_update(simplex,f_values,d,high,low,centroid)
  end subroutine simplex_initiate    

  function downhill_simplex(f,simplex,d,simplex_size_goal) result(k)
    interface
       function f(x)
         real*8 x(:), f
       end function f
    end interface
    real*8 simplex(:,:), centroid(d), F_value(d+1), p1(d), p2(d), f_re, f_ex, f_co, simplex_size_goal
    integer i, k, d, high, low

    k = 0
    call simplex_initiate(f,simplex,F_value,d,high,low,centroid)

    do while (size(simplex,d) .gt. simplex_size_goal)
       call simplex_update(simplex,F_value,d,high,low,centroid)
       call reflection(simplex(high,:),centroid,d,p1)
       f_re = f(p1)
       if (f_re .lt. F_value(low)) then
          call expansion(simplex(high,:),centroid,d,p2)
          f_ex = F(p2)
          if (f_ex .lt. f_re) then
             do i=1,d
                simplex(high,i) = p2(i)             
             end do
             F_value(high) = f_ex       
          else             
             do i=1,d                
                simplex(high,i) = p1(i)                
             end do             
             F_value(high) = f_re
          end if          
       else          
          if (f_re .lt. F_value(high)) then
             do i=1,d
                simplex(high,i) = p1(i)
             end do
             F_value(high) = f_re             
          else             
             call contraction(simplex(high,:),centroid,d,p1)             
             f_co = f(p1)             
             if (f_co .lt. F_value(high)) then                
                do i=1,d                   
                   simplex(high,i) = p1(i)                   
                end do                
                F_value(high) = f_co                             
             else                
                call reduction(simplex,d,low)                
                call simplex_initiate(f,simplex,F_value,d,high,low,centroid)                
             end if
          end if
       end if       
          k = k + 1          
       end do       
     end function downhill_simplex     
                      
end module minimization

  
       
       
         
  
