program output
  implicit none
  real*8 x
  integer i, n, stat
  character(len=32) :: arg

  open(unit=1, file='data.dat')

  call get_command_argument(1,arg)
  read(arg,*,iostat=stat) n
  
  do i=1,n
     read(1,*) x
     write(*,"(F6.3,x,F6.3)") x, cos(x)     
  end do

end program output
