module linspline
  implicit none

contains

  function lspline(x,y,z) result(s)
    real*8 s, x(:), y(:), z, ai, bi
    integer i, j, m

    i = 1
    j = size(x)

    do while ((j-i) .gt. 1)
       m = (i+j)/2
       if (z .ge. x(m)) then
          i = m
       else
          j = m
       end if
    end do

    ai = y(i)
    bi = (y(i+1) - y(i))/(x(i+1) - x(i))
    s = ai + bi*(z-x(i))
  end function lspline

  function lint(x,y,z) result(w)
    real*8 x(:), y(:), z, w, bi, ai, h
    integer i

    i = 1
    w = 0

  do while (z .gt. x(i+1))
    h=x(i+1)-x(i)
    bi = (y(i+1) - y(i))/h
    w = w + y(i)*h + 0.5*bi*h*h
    i = i + 1
  end do
    h = z - x(i)
    bi = (y(i+1) - y(i))/(x(i+1) - x(i))
    w = w + y(i)*h + 0.5*bi*h*h

  end function lint

end module linspline

       
       
