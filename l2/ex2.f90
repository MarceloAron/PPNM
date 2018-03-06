program ex2
  implicit none
  real*4 x
  real*8 y
  real*16 z

  x = 0.1111111111111111111111111111q0
  y = 0.1111111111111111111111111111q0
  z = 0.1111111111111111111111111111q0

  print*, "Significant digits:"

  print*, "Float:", x
  print*, "Double:", y
  print*, "Long double:", z

end program ex2

  
