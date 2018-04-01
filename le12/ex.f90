program error
  implicit none

  integer x0, tf
  real dt, xex, xanalit, t, pi, x, xaux, fx

!  read*, t, tf, dt

  t = -3
  tf = 3
  dt = 0.01
  x0 = -1
  pi = 3.14159265359

  xex = x0
  x = x0

  do while (t<tf)
     xex = xex +(2/sqrt(pi))*exp(-t**2)*dt

     xanalit = erf(t)

     fx = (2/sqrt(pi))*exp(-t**2)
     xaux = x + (fx*dt)/2

     x = x + fx*dt

     t = t + dt
     
     print*, t, xex, xanalit, x
     
  end do
end program error
