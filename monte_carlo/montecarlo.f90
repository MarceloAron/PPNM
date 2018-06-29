module montecarlo
  implicit none

contains

  subroutine randx(a,b,x,dim)
    real*8 a(:), b(:), x(:), r
    integer i, dim

    do i=1,dim
       call random_number(r)
       x(i) = a(i) + r*(b(i)-a(i))
    end do
  end subroutine randx

  subroutine plain_montecarlo(f,a,b,dim,N,res,err)
    real*8 a(:), b(:), res, err
    integer i, dim, N
    interface
       function f(x)
         real*8 x(:), f
       end function f
    end interface
    real*8 sum, sum2, avr, var, V, fx, x(dim)

    V = 1
    sum = 0
    sum2 = 0

    do i=1,dim
       V = V*(b(i) - a(i))
    end do    
    
    do i=1,N
       call randx(a,b,x,dim)
       fx = f(x)
       sum = sum + fx
       sum2 = sum2 + fx*fx
    end do

    avr = sum/N
    var = sum2/N - avr*avr

    res = avr*V
    err = sqrt(var/N)*V
  end subroutine plain_montecarlo  

end module montecarlo
