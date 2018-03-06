program ex2
  implicit none
  real g, b
  real*8 e, pi
  complex r, i, n, x, s, k, l
  !  integer 

  g = GAMMA(5.d0)

  b = bessel_j1(0.5)

  r = (1,0)
  i = (0,1)
  n = (-2,0)
  x = sqrt(n)

  s = exp(i)

  pi = 3.14159265
  k = exp(i*pi)

  e = exp(1.d0)
  l = i**e  

  print*, "The gamma function of 5 is:", g
  print*, "The bessel function of the first kind of order 1 of 0.5 is:", b
  print*, "The square root of -2 is:", x
  print*, "The euler number elevated in the 'i'th power is:", s
  print*, "The euler number elevated in the 'i*pi'th power is:", k
  print*, "The imaginary number elevated in the 'e'th power is:", l
  
end program ex2
