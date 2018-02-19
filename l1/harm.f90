function f(x)
  implicit none
  real*8  f
  integer x

  f = 1.d0/x

end function f

program harm
  implicit none
  integer i, n, x
  real*8 s
  real*8, external :: f

  s = 0
  
  print*, s
  
  do i=1,10000000
     s = s + f(i)
  end do

  print*, s

end program harm


     
