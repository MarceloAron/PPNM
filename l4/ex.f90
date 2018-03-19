program ex
use module_komplex
  implicit none
  type (komplex) :: a
  type (komplex) :: b
  type (komplex) :: r
  type (komplex) :: q
  type (komplex) :: m
  type (komplex) :: d
  type (komplex) :: ca
  type (komplex) :: cb
  type (komplex) :: ta
  type (komplex) :: tb  
  
  a = komplex(1.,2.)
  b = komplex(3.,4.)

  print*, "a=", a
  print*, "b=", b
  
  r = komplex_add(a,b)
  q = komplex_sub(a,b)
  m = komplex_mul(a,b)
  d = komplex_div(a,b)
  ca = komplex_conjugate(a)
  cb = komplex_conjugate(b)
  ta = komplex_abs(a)
  tb = komplex_abs(b)
  print*, "a+b=", r
  print*, "a-b=", q
  print*, "a*b=", m
  print*, "a/b=", d
  print*, "complex conjugate of a=", ca
  print*, "complex conjugate of b=", cb
  print*, "absolute value of a=", ta
  print*, "absolute value of b=", tb

  print*, "Now we will just test the komplex_print structure (it is not necessary to print complex numbers in Fortran):" 
  call komplex_print("a+b=",r)
  print*, "The result is the same as obtained with the normal print."

end program ex


   
