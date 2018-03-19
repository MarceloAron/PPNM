module module_komplex

type komplex
real:: re,im
end type komplex

contains

subroutine komplex_print(s,z)
  character(LEN=*) :: s
  type (komplex) :: z

  print*, s, z
  
end subroutine komplex_print

subroutine komplex_set(z,x,y)
  implicit none
  real x,y
  type (komplex) :: z

  z%re = x
  z%im = y
  
end subroutine komplex_set

function komplex_new(x,y)
  implicit none
  type (komplex) :: komplex_new
  real x,y

  type (komplex) :: z

  komplex_new = komplex(x,y)

end function komplex_new

function komplex_add(a,b)
  implicit none
  type (komplex) :: komplex_add
  type (komplex) :: a
  type (komplex) :: b

  komplex_add = komplex(a%re + b%re, a%im + b%im)

end function komplex_add

function komplex_sub(a,b)
  implicit none
  type (komplex) :: komplex_sub
  type (komplex) :: a
  type (komplex) :: b

  komplex_sub = komplex(a%re - b%re,a%im - b%im)

end function komplex_sub

function komplex_mul(a,b)
  implicit none
  type (komplex) :: komplex_mul
  type (komplex) :: a
  type (komplex) :: b

  komplex_mul = komplex(a%re*b%re - a%im*b%im,a%re*b%im + a%im*b%re)

end function komplex_mul

function komplex_div(a,b)
  implicit none
  type (komplex) :: komplex_div
  type (komplex) :: a
  type (komplex) :: b

  komplex_div = komplex((a%re*b%re + a%im*b%im)/(b%re*b%re + b%im*b%im),(a%im*b%re - a%re*b%im)/(b%re*b%re + b%im*b%im))

end function komplex_div

function komplex_conjugate(z)
  implicit none
  type (komplex) :: komplex_conjugate
  type (komplex) :: z

  komplex_conjugate = komplex(z%re,-z%im)

end function komplex_conjugate

function komplex_abs(z)
  implicit none
  type (komplex) :: komplex_abs
  type (komplex) :: z

  komplex_abs = komplex(z%re*z%re + z%im*z%im, 0.)

end function komplex_abs
  
end module module_komplex
