module fitfuns
  implicit none
  integer, parameter :: m=3

contains
  function f(i,x)
    integer, intent(in) :: i
    real*8, intent(in) :: x
    real*8 :: f

    select case(i)
    case(1)
       f = log10(x)
    case(2)
       f = 1
    case(3)
       f = x
    case default
       f = (x-x)/(x-x)
    end select
  end function f

  function fit(c,x)
    real*8, intent(in) :: x, c(m)
    real*8 :: fit
    integer i
    fit = 0

   do i=1,m
      fit = fit + c(i)*f(i,x)
   end do   
 end function fit
 
end module fitfuns
program A1
  use least_squares
  use fitfuns
  implicit none
  real*8, allocatable :: x(:), y(:), dy(:), c(:), c1_minus(:), c2_minus(:), c3_minus(:), c1_plus(:), c2_plus(:), c3_plus(:), S(:,:)
  integer i, n
  real*8 z, dz

  x = [0.1,1.33,2.55,3.78,5.,6.22,7.45,8.68,9.9]
  y = [-15.3,0.32,2.45,2.75,2.27,1.35,0.157,-1.23,-2.75]
  dy = [1.04,0.594,0.983,0.998,1.11,0.398,0.535,0.968,0.478]

  n = size(x)

  do i=1,n
     print*, x(i), y(i), dy(i)
  end do

  print*
  print*

  allocate(S(m,m))

  c = lsfit(m,f,x,y,dy,S)
  c1_minus = c
  c2_minus = c
  c3_minus = c
  c1_plus = c
  c2_plus = c
  c3_plus = c
  c1_minus(1) = c1_minus(1) - sqrt(S(1,1))
  c2_minus(2) = c2_minus(2) - sqrt(S(2,2))
  c3_minus(3) = c3_minus(3) - sqrt(S(3,3))
  c1_plus(1) = c1_plus(1) + sqrt(S(1,1))
  c2_plus(2) = c2_plus(2) + sqrt(S(2,2))
  c3_plus(3) = c3_plus(3) + sqrt(S(3,3))

  dz = (x(n) - x(1))/64
  z = x(1)

  do while (z .lt. (x(n)+dz))
     print*, z, fit(c,z), fit(c1_minus,z), fit(c2_minus,z), fit(c3_minus,z), fit(c1_plus,z), fit(c2_plus,z), fit(c3_plus,z)
     z = z + dz
  end do

end program A1

  
  
