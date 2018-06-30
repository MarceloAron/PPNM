module functions
  implicit none

contains

  function activ(x) result(f)
    real*8 x, f
    f = x*exp(-x*x)
  end function activ

  function fit(x) result(f)
    real*8 x, f
    f = cos(5*x-1)*exp(-x*x)
  end function fit

end module functions

program main
  use functions
  use ann
  use qr_givens
  implicit none
  real*8 a, b, x, f, z, dz, y
  real*8, allocatable :: xlist(:), ylist(:)
  integer i, n, nx
  type (neuron) :: network

  n = 5
  network = ann_alloc(n)
  a = -1
  b = 1
  nx = 20

  allocate(xlist(nx),ylist(nx))

  do i=1,nx
     x = a + (b-a)*i/(nx-1)
     f = fit(x)
     xlist(i) = x
     ylist(i) = f
  end do

  do i=1,network%n
     network%data(3*i+0) = a + (b-a)*i/(network%n-1)
     network%data(3*i+1) = 1
     network%data(3*i+2) = 1
  end do

  write(*,*) "Hello"
  call ann_train(network,xlist,ylist,activ)
  write(*,*) "Ok this passed"
  
  do i=1,size(ylist)
     x = xlist(i)
     f = ylist(i)
     write(0,*) x, f
  end do

  write(0,*)
  write(0,*)

  dz = 1d0/64d0
  z = a
  do while (z .le. b)
     y = ann_feed_forward(network,z,fit)
     write(0,*) z, y
     z = z + dz
  end do

end program main
