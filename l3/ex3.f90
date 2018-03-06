subroutine f(a, b, tau, eps)
  implicit none
  real*8 a, b, tau, eps

  if (abs(a-b)<tau) then
     print*, "1"
  else if (abs(a-b)/(abs(a)+abs(b))<(eps/2)) then
     print*, "1"
  else   
     print*, "0"
  end if

end subroutine f

program ex3
  implicit none
  real*8 a, b, tau, eps
    
  a = 1.00001d0
  b = 1.00002d0
  tau = 0.0001d0
  eps = 0.00001d0

  print*, "With a=", a, "b=", b, "tau=", tau, "and eps=", eps

  call f(a, b, tau, eps)

  a = 1.01d0
  b = 1.02d0
  tau = 0.001d0
  eps = 0.01d0

  print*, "With a=", a, "b=", b, "tau=", tau, "and eps=", eps

  call f(a, b, tau, eps)

  a = 1.1d0
  b = 1.2d0
  tau = 0.01d0
  eps = 0.01d0

  print*, "With a=", a, "b=", b, "tau=", tau, "and eps=", eps

  call f(a, b, tau, eps)
    
end program ex3
