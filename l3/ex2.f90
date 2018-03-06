program ex2
  implicit none
  real*8 s, r
  integer max, i, n

  max = 2147483647

  i = max/2

  s = 0

  do n=1,i
     s = s + 1.d0/n
  end do

  print*, "The sum of 1/x from 1 to 1073741823 is:", s

  r = 0

  do n=i,1,-1
     r = r + 1.d0/n
  end do

  print*, "The sum of 1/x from 1073741823 to 1 is:", r

end program ex2
