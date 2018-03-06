program ex1
  implicit none
  integer i, j, n
  real*4 s
  real*8 f
  real*16 r
  
  i = 0
  j = 0

  do while (i+1>i)
     i = i + 1
  end do

  do while (j-1<j)
     j = j - 1
  end do

  print*, "The lowest integer is:", j
  print*, "The highest integer is:", i
  
  s=1.e0

  do while (1.e0 + s > 1.e0)
     s=s/2
  end do
  
  s=s*2
  
  print*, "The machine epsilon real*4 is:", s

  f=1.d0
  
  do while (1.d0 + f > 1.d0)
     f=f/2
  end do

  f=f*2
  
  print*, "The machine epsilon real*8 is:", f

  r = 1.q0

  do while (1.d0 + r > 1.q0)
     r=r/2
  end do

  r=r*2

  print*, "The machine epsilon real*16 is:", r
  

end program ex1
