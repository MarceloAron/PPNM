module cubespline
  implicit none

  type cspline
     real*8, allocatable, dimension(:) :: x, y, b, c, d
  end type cspline

contains

  function cspline_alloc(x,y) result (s)
    real*8 x(:), y(:)
    real*8, allocatable :: p(:), h(:), De(:), Q(:), Be(:)
    type (cspline) :: s
    integer i, n

    n = size(x)

    allocate(s%x(n),s%y(n),s%b(n),s%c(n-1),s%d(n-1),p(n-1),h(n-1),De(n),Q(n-1),Be(n))

    s%x(:) = x(:)
    s%y(:) = y(:)

    do i=1,n-1
       h(i) = x(i+1) - x(i)
       p(i) = (y(i+1) - y(i))/h(i)
    end do

    De(1) = 2.
    De(n) = 2.
    Q(1) = 1.
    Be(1) = 3.*p(1)
    Be(n) = 3.*p(n-1)

    do i=1,n-2
       De(i+1) = 2.*(h(i)/h(i+1)) + 2.
       Q(i+1) = h(i)/h(i+1)
       Be(i+1) = 3.*(p(i) + p(i+1)*Q(i+1))       
    end do

    do i=2,n
       De(i) = De(i) - Q(i-1)/De(i-1)
       Be(i) = Be(i) - Be(i-1)/De(i-1)
    end do

    s%b(n) = Be(n)/De(n)

    do i=n-1,1,-1
       s%b(i) = (Be(i) - Q(i)*s%b(i+1))/De(i)
    end do

    do i=1,n-1
       s%c(i) = (-2*s%b(i) - s%b(i+1) + 3*p(i))/h(i)
       s%d(i) = (s%b(i) + s%b(i+1) - 2*p(i))/(h(i)*h(i))
    end do

    deallocate(p,h)
  end function cspline_alloc

  function cspline_eval(s,z) result(f)
    real*8 z, f, h
    integer i, j, m
    type (cspline) :: s

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
    f = s%y(i) + h*(s%b(i) + h*(s%c(i) + h*s%d(i)))
  end function cspline_eval

  function cspline_int(s,z) result(w)
    real*8 z, w, h
    integer i
    type (cspline) :: s

    i = 1
    w = 0

    do while (z .gt. s%x(i+1))
       h = s%x(i+1) - s%x(i)
       w = w + s%y(i)*h + 0.5*s%b(i)*h*h + (1./3.)*s%c(i)*h*h*h + 0.25*s%d(i)*h*h*h*h
       i = i + 1
    end do
    h = z - s%x(i)    
    w = w + s%y(i)*h + 0.5*s%b(i)*h*h + (1./3.)*s%c(i)*h*h*h + 0.25*s%d(i)*h*h*h*h
  end function cspline_int 

  function cspline_der(s,z) result(k)
    real*8 z, k, h
    integer i, j, m
    type (cspline) :: s

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
    k = s%b(i) + 2*s%c(i)*h + 3*s%d(i)*h*h
  end function cspline_der 
    
end module cubespline

    
       
    
