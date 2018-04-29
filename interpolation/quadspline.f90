module quadspline
  implicit none

  type qspline
     real*8, allocatable, dimension(:) :: x, y, b, c
  end type qspline

contains

  function qspline_alloc(x,y) result(s)
    real*8 x(:), y(:)
    real*8, allocatable :: p(:), h(:)
    type (qspline) :: s
    integer i, n

    n = size(x)

    allocate(s%x(n), s%y(n), s%b(n-1), s%c(n-1), p(n-1), h(n-1))

    s%x(:) = x(:)
    s%y(:) = y(:)

    do i=1,n-1
       h(i) = x(i+1) - x(i)
       p(i) = (y(i+1) - y(i))/h(i)
    end do

    s%c(1) = 0 !We choose c1 = 0 so we can calculate recursively the other cn

    do i=1,n-2
       s%c(i+1) = (p(i+1) - p(i) - s%c(i)*h(i))/h(i+1)
    end do

    s%c(n-1) = s%c(n-1)/2 !Now we do backward-recursion

    do i=n-2,1,-1
       s%c(i) = (p(i+1) - p(i) - s%c(i+1)*h(i+1))/h(i)
    end do

    do i=1,n-1
       s%b(i) = p(i) - s%c(i)*h(i)
    end do

    deallocate(p,h)
  end function qspline_alloc

  function qspline_eval(s,z) result(f)
    real*8 z, f, h
    integer i, j, m
    type (qspline) :: s

    i = 1
    j = size(s%x)

    do while((j-i) .gt. 1)
       m = (i+j)/2
       if (z .ge. s%x(m)) then
          i = m
       else
          j = m
       end if
    end do
    h = z - s%x(i)
    f = s%y(i) + s%b(i)*h + s%c(i)*h*h
  end function qspline_eval

  function qspline_int(s,z) result (w)
    real*8 z, w, h 
    integer i
    type (qspline) :: s

    i = 1
    w = 0

    do while (z .gt. s%x(i+1))
       h = s%x(i+1) - s%x(i)       
       w = w + s%y(i)*h + 0.5*s%b(i)*h*h + (1./3.)*s%c(i)*h*h*h
       i = i + 1
    end do
       h = z - s%x(i)
       w = w + s%y(i)*h + 0.5*s%b(i)*h*h + (1./3.)*s%c(i)*h*h*h
     end function qspline_int

     function qspline_der(s,z) result(k)
       real*8 z, k, h
       integer i, j, m
       type (qspline) :: s
           
        i = 1
        j = size(s%x)        

        do while((j-i) .gt. 1)          
           m = (i+j)/2           
           if (z .ge. s%x(m)) then
              i = m                           
           else              
              j = m              
           end if           
        end do

        h = z - s%x(i)
        k = s%b(i) + 2*s%c(i)*h
      end function qspline_der      
    
end module quadspline

    
    
