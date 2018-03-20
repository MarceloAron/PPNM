module module_nvector

  type nvector
     integer size
     real*8, allocatable :: data(:)
  end type nvector

contains

  subroutine nvector_alloc(v,n)
    implicit none
    integer n
    type (nvector) :: v

    allocate (v%data (n))
    v%size = n

  end subroutine nvector_alloc

  subroutine nvector_dealloc(v)
    implicit none
    type (nvector) :: v

    deallocate (v%data)
    v%size = 0

  end subroutine nvector_dealloc

  subroutine nvector_set(v,i,val)
    implicit none
    integer i
    real val
    type (nvector) :: v

    v%data(i) = val

  end subroutine nvector_set

  function nvector_get(v,i)
    implicit none
    integer i
    type (nvector) :: v
    real nvector_get

    nvector_get = v%data(i)

  end function nvector_get
  
  function nvector_dot_product(v,u)
    implicit none
    type (nvector) :: v
    type (nvector) :: u
    real nvector_dot_product

    nvector_dot_product = dot_product(u%data,v%data)

  end function nvector_dot_product

  subroutine nvector_set_zero(v)
    implicit none
    type (nvector) :: v

    v%data = 0

  end subroutine nvector_set_zero

  function nvector_add(n,v,u)
    implicit none
    integer n
    type (nvector) :: v
    type (nvector) :: u
    type (nvector) :: nvector_add

    nvector_add = nvector(n, v%data + u%data)

  end function nvector_add
  
  function nvector_sub(n,v,u)
    implicit none
    integer n
    type (nvector) :: v
    type (nvector) :: u
    type (nvector) :: nvector_sub

    nvector_sub = nvector(n, v%data - u%data)

  end function nvector_sub
  
end module module_nvector
