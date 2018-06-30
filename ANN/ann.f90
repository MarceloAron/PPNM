module ann
  use minimization
  implicit none

!  type neuron
!     integer n
!     interface
!        function f(x)
!          real*8 x, f
!        end function f
!     end interface
!     real*8, allocatable :: data(:)
!  end type neuron

contains  

  function ann_alloc(n) result(network)
!    interface       
!       function f(x)         
!         real*8 x, f         
!       end function f       
!    end interface    
    integer n
    type (neuron) :: network

    network%n = n
!   network%f => f 
    allocate(network%data(3*n))    
    
  end function ann_alloc

  subroutine ann_free(network)
    type (neuron) :: network
    deallocate(network%data)    
  end subroutine ann_free

  function ann_feed_forward(network,x,f) result(s)
    interface
       function f(x)
         real*8 x, f
       end function f
    end interface  
    real*8 x, s, a, b, w
    type (neuron) :: network
    integer i

    s = 0

    do i=1,network%n
       a = network%data(3*i+0)
       b = network%data(3*i+1)
       w = network%data(3*i+2)
!       network%f = f((x-a)/b)
       s = s + f((x-a)/b)*w
    end do    

  end function ann_feed_forward

  function delta(p,network,xlist,ylist,func) result(s)
    interface
       function func(x)
         real*8 x, func
       end function func
    end interface    
    real*8 p(:), x, f, y, s, xlist(:), ylist(:)    
    type (neuron) :: network
    integer i

    network%data = p
    s = 0

    do i=1,size(xlist)
       x = xlist(i)
       f = ylist(i)
       y = ann_feed_forward(network,x,func)
       s = s + abs(y-f)
    end do
    s = s/size(xlist)
  end function delta  

  subroutine ann_train(network,xlist,ylist,func)
    interface
       function func(x)
         real*8 x, func
       end function func
    end interface    
    real*8 xlist(:), ylist(:), eps
    real*8, allocatable :: p(:)
    integer steps
    type (neuron) :: network

    eps = 1d-6

    allocate(p(size(network%data)))
    p = network%data
    write(*,*) "Hello 2"
    call quasi_newton(delta,p,eps,steps,network,xlist,ylist,func)
    write(*,*) "Oh, so wee passed 2"
    network%data = p
    deallocate(p)        
    
  end subroutine ann_train

end module ann

  
