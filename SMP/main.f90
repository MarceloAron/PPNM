module functions
  implicit none
  integer calls

contains

  function integ_one(x) result(f)
    real*8 x, f
    calls = calls + 1
    f = sqrt(x)
  end function integ_one

  function integ_two(x) result(f)
    real*8 x, f
    calls = calls + 1    
    f = 1/sqrt(x)
  end function integ_two

  function integ_three(x) result(f)
    real*8 x, f
    calls = calls + 1    
    f = log(x)/sqrt(x)
  end function integ_three

  function integ_four(x) result(f)
    real*8 x, f
    calls = calls + 1
    f = 4*sqrt(1-(1-x)**2)
  end function integ_four

  function integ_five(x) result(f)
    real*8 x, f
    calls = calls + 1
    f = (x**2)*exp(-x**2)
  end function integ_five

  function integ_six(x) result(f)
    real*8 x, f
    calls = calls + 1
    f = 1/(x**3 - 1)
  end function integ_six

  function integ_seven(x) result(f)
    real*8 x, f
    calls = calls + 1
    f = 1/(x**2 + 4)
  end function integ_seven

  function integ_eight(x) result(f)
    real*8 x, f
    calls = calls + 1
    f = sin(x)/x
  end function integ_eight

  function integ_nine(x) result(f)
    real*8 x, f
    calls = calls + 1
    f = 1/sqrt(x**2 + 1)
  end function integ_nine  

end module functions

program main
  use integration
  use functions
  use ieee_arithmetic
  implicit none
  real*8 a, b, acc, eps, Q, exact, err, start, finish
  real*8 Q1, Q2, Q3, Q4, a1, a2, a3, b1, b2, b3, exact1, exact2, exact3
  integer i, c

  write(*,*) "Using OMP in the integration exercise"  
  write(*,*) "Parallel execution (each processor doing a subexercise):"

  acc = 1d-9
  eps = 1d-9
  call cpu_time(start)
  write(*,"(x,A19,x,F11.9)") "Absolute precision:", acc
  write(*,"(x,A19,x,F11.9)") "Relative precision:", eps
  
  !$omp parallel sections private(i)
  !$omp section 
  a1 = 0
  b1 = 1
  exact1 = 2d0/3d0
  Q1 = adapt(integ_one,a1,b1,acc,eps,err)
  !$omp section
  a2 = ieee_value(a, ieee_negative_inf)
  b2 = ieee_value(b, ieee_positive_inf)
  exact2 = sqrt(3.141592653589793d0)/2d0
  Q2 = adapt_inf(integ_five,a2,b2,acc,eps,err)
  !$omp section
  a3 = 0
  b3 = 1
  exact3 = 2d0
  Q3 = adapt(integ_two,a3,b3,acc,eps,err)
  Q4 = adapt_clenshaw_curtis(integ_two,a3,b3,acc,eps,err)
  !$omp end parallel sections

  write(*,*) "======================================================================"
  write(*,*) "                              Exercise A                              "
  write(*,*) "======================================================================"

  write(*,*) "Function = sqrt(x)"
  write(*,"(x,A9,x,F3.1,x,A2,x,F3.1)") "Interval:", a1, "to", b1
  write(*,"(x,A10,x,F11.9)") "Integral =", Q1
  write(*,"(x,A10,x,F11.9)") "Should be:", exact1

  write(*,*) "======================================================================"  
  write(*,*) "                              Exercise B                              "
  write(*,*) "======================================================================"

  write(*,*) "Function = (x**2)*exp(-x**2)"
  write(*,"(x,A9,x,F9.0,x,A2,x,F8.0)") "Interval:", a2, "to", b2
  write(*,"(x,A10,x,F11.9)") "Integral =", Q2
  write(*,"(x,A10,x,F11.9)") "Should be:", exact2

  write(*,*) "======================================================================"  
  write(*,*) "                              Exercise C                              "
  write(*,*) "======================================================================"

  write(*,*) "Function = 1/sqrt(x)"
  write(*,"(x,A9,x,F3.1,x,A2,x,F3.1)") "Interval:", a3, "to", b3
  write(*,"(x,A10,x,F11.9,2x,A8)") "Integral =", Q3, "(Normal)"
  write(*,"(x,A10,x,F11.9,2x,A17)") "Integral =", Q4, "(Clenshaw-Curtis)"  
  write(*,"(x,A10,x,F11.9)") "Should be:", exact3

  write(*,*)

  call cpu_time(finish)
  write(*,"(x,A5,x,F9.7,x,A7)") "Time:", finish-start, "seconds"

  write(*,*)
  write(*,*)

  write(*,*) "Single thread execution:"
  acc = 1d-9
  eps = 1d-9
  call cpu_time(start)

  write(*,"(x,A19,x,F11.9)") "Absolute precision:", acc
  write(*,"(x,A19,x,F11.9)") "Relative precision:", eps

  a1 = 0  
  b1 = 1
  exact1 = 2d0/3d0
  Q1 = adapt(integ_one,a1,b1,acc,eps,err)

  a2 = ieee_value(a, ieee_negative_inf)
  b2 = ieee_value(b, ieee_positive_inf)
  exact2 = sqrt(3.141592653589793d0)/2d0
  Q2 = adapt_inf(integ_five,a2,b2,acc,eps,err)
  
  a3 = 0
  b3 = 1
  exact3 = 2d0
  Q3 = adapt(integ_two,a3,b3,acc,eps,err)
  Q4 = adapt_clenshaw_curtis(integ_two,a3,b3,acc,eps,err)

   write(*,*) "======================================================================"
  write(*,*) "                              Exercise A                              "
  write(*,*) "======================================================================"

  write(*,*) "Function = sqrt(x)"
  write(*,"(x,A9,x,F3.1,x,A2,x,F3.1)") "Interval:", a1, "to", b1
  write(*,"(x,A10,x,F11.9)") "Integral =", Q1
  write(*,"(x,A10,x,F11.9)") "Should be:", exact1

  write(*,*) "======================================================================"  
  write(*,*) "                              Exercise B                              "
  write(*,*) "======================================================================"

  write(*,*) "Function = (x**2)*exp(-x**2)"
  write(*,"(x,A9,x,F9.0,x,A2,x,F8.0)") "Interval:", a2, "to", b2
  write(*,"(x,A10,x,F11.9)") "Integral =", Q2
  write(*,"(x,A10,x,F11.9)") "Should be:", exact2

  write(*,*) "======================================================================"  
  write(*,*) "                              Exercise C                              "
  write(*,*) "======================================================================"

  write(*,*) "Function = 1/sqrt(x)"
  write(*,"(x,A9,x,F3.1,x,A2,x,F3.1)") "Interval:", a3, "to", b3
  write(*,"(x,A10,x,F11.9,2x,A8)") "Integral =", Q3, "(Normal)"
  write(*,"(x,A10,x,F11.9,2x,A17)") "Integral =", Q4, "(Clenshaw-Curtis)"  
  write(*,"(x,A10,x,F11.9)") "Should be:", exact3

  write(*,*)

  call cpu_time(finish)
  write(*,"(x,A5,x,F9.7,A7)") "Time:", finish-start, "seconds"

end program main
