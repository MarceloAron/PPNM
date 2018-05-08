program cmdline
  implicit none
  real*8 x
  integer stat
  character(len=32) :: arg

  call get_command_argument(1,arg)
  read(arg,*,iostat=stat) x

  write(*,"(F6.3,x,F6.3)") x, sin(x)

end program cmdline
