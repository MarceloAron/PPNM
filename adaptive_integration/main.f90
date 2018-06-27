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
  real*8 a, b, acc, eps, Q, exact, err, Q1, Q2
  integer c

  write(*,*) "======================================================================"
  write(*,*) "                              Exercise A                              "
  write(*,*) "======================================================================"

  acc = 1d-6
  eps = 1d-6

  calls = 0
  write(*,*) "Function = sqrt(x)"  
  a = 0
  b = 1
  exact = 2./3.
  write(*,"(x,A9,x,F3.1,x,A2,x,F3.1)") "Interval:", a, "to", b
  Q = adapt(integ_one,a,b,acc,eps,err)
  write(*,"(x,A10,x,F11.9)") "Integral =", Q
  write(*,"(x,A10,x,F11.9)") "Should be:", exact
  write(*,"(x,A16,x,F11.9)") "Estimated error:", err
  write(*,"(x,A13,4x,F11.9)") "Actual error:", abs(Q-exact)
  write(*,"(x,A17,x,I3)") "Number of calls =", calls

  write(*,*)
  write(*,*) "----------------------------------------------------------------------"
  write(*,*)

  calls = 0
  write(*,*) "Function = 1/sqrt(x)"
  a = 0
  b = 1
  exact = 2
  write(*,"(x,A9,x,F3.1,x,A2,x,F3.1)") "Interval:", a, "to", b
  Q = adapt(integ_two,a,b,acc,eps,err)
  write(*,"(x,A10,x,F11.9)") "Integral =", Q
  write(*,"(x,A10,x,F11.9)") "Should be:", exact
  write(*,"(x,A16,x,F11.9)") "Estimated error:", err
  write(*,"(x,A13,4x,F11.9)") "Actual error:", abs(Q-exact)
  write(*,"(x,A17,x,I5)") "Number of calls =", calls  

  write(*,*)
  write(*,*) "----------------------------------------------------------------------"
  write(*,*)

  calls = 0
  write(*,*) "Function = ln(x)/sqrt(x)"
  a = 0
  b = 1
  exact = -4
  write(*,"(x,A9,x,F3.1,x,A2,x,F3.1)") "Interval:", a, "to", b
  Q = adapt(integ_three,a,b,acc,eps,err)
  write(*,"(x,A10,x,F12.9)") "Integral =", Q
  write(*,"(x,A10,x,F12.9)") "Should be:", exact
  write(*,"(x,A16,x,F11.9)") "Estimated error:", err
  write(*,"(x,A13,4x,F11.9)") "Actual error:", abs(Q-exact)
  write(*,"(x,A17,x,I6)") "Number of calls =", calls

  write(*,*)
  write(*,*) "----------------------------------------------------------------------"
  write(*,*)

  calls = 0
  write(*,*) "Function = 4*sqrt(1-(1-x)**2)"
  a = 0
  b = 1
  exact = 3.141592653589793d0
  acc = 1d-12
  eps = 1d-12
  write(*,"(x,A9,x,F3.1,x,A2,x,F3.1)") "Interval:", a, "to", b
  Q = adapt(integ_four,a,b,acc,eps,err)
  write(*,"(x,A10,x,F17.15)") "Integral =", Q
  write(*,"(x,A10,x,F17.15)") "Should be:", exact
  write(*,"(x,A16,x,F17.15)") "Estimated error:", err
  write(*,"(x,A13,4x,F17.15)") "Actual error:", abs(Q-exact)
  write(*,"(x,A17,x,I6)") "Number of calls =", calls

  write(*,*) "======================================================================"  
  write(*,*) "                              Exercise B                              "
  write(*,*) "======================================================================"

  acc = 1d-6
  eps = 1d-6

  calls = 0
  write(*,*) "Function = (x**2)*exp(-x**2)"  
  a = ieee_value(a, ieee_negative_inf)
  b = ieee_value(b, ieee_positive_inf)
  exact = sqrt(3.141592653589793d0)/2d0
  write(*,"(x,A9,x,F9.0,x,A2,x,F8.0)") "Interval:", a, "to", b
  Q = adapt_inf(integ_five,a,b,acc,eps,err)
  write(*,"(x,A10,x,F11.9)") "Integral =", Q
  write(*,"(x,A10,x,F11.9)") "Should be:", exact
  write(*,"(x,A16,x,F11.9)") "Estimated error:", err
  write(*,"(x,A13,4x,F11.9)") "Actual error:", abs(Q-exact)
  write(*,"(x,A17,x,I4)") "Number of calls =", calls

  write(*,*)
  write(*,*) "----------------------------------------------------------------------"
  write(*,*)

  calls = 0  
  write(*,*) "Function = 1/(x**3 - 1)"  
  a = ieee_value(a, ieee_negative_inf)
  b = 0
  exact = -1.209199576d0
  write(*,"(x,A9,x,F9.0,x,A2,x,F3.1)") "Interval:", a, "to", b
  Q = adapt_inf(integ_six,a,b,acc,eps,err)
  write(*,"(x,A10,x,F12.9)") "Integral =", Q
  write(*,"(x,A10,x,F12.9)") "Should be:", exact
  write(*,"(x,A16,x,F11.9)") "Estimated error:", err
  write(*,"(x,A13,4x,F11.9)") "Actual error:", abs(Q-exact)
  write(*,"(x,A17,x,I3)") "Number of calls =", calls

  write(*,*)
  write(*,*) "----------------------------------------------------------------------"
  write(*,*)

  calls = 0  
  write(*,*) "Function = 1/(x**2 + 4)"  
  a = 0
  b = ieee_value(b, ieee_positive_inf)
  exact = 3.141592653589793d0/4d0
  write(*,"(x,A9,x,F3.1,x,A2,x,F8.0)") "Interval:", a, "to", b
  Q = adapt_inf(integ_seven,a,b,acc,eps,err)
  write(*,"(x,A10,x,F12.10)") "Integral =", Q
  write(*,"(x,A10,x,F12.10)") "Should be:", exact
  write(*,"(x,A16,x,F12.10)") "Estimated error:", err
  write(*,"(x,A13,4x,F12.10)") "Actual error:", abs(Q-exact)
  write(*,"(x,A17,x,I3)") "Number of calls =", calls

  write(*,*) "======================================================================"  
  write(*,*) "                              Exercise C                              "
  write(*,*) "======================================================================"

  calls = 0
  write(*,*) "Function = 1/sqrt(x)"
  a = 0
  b = 1
  exact = 2d0
  write(*,"(x,A9,x,F3.1,x,A2,x,F3.1)") "Interval:", a, "to", b  
  Q1 = adapt(integ_two,a,b,acc,eps,err)
  write(*,"(x,A10,x,F11.9,2x,A8)") "Integral =", Q1, "(Normal)"
  c = calls
  calls = 0
  Q2 = adapt_clenshaw_curtis(integ_two,a,b,acc,eps,err)
  write(*,"(x,A10,x,F11.9,2x,A17)") "Integral =", Q2, "(Clenshaw-Curtis)"  
  write(*,"(x,A10,x,F11.9)") "Should be:", exact
  write(*,"(x,A16,x,F11.9)") "Estimated error:", err
  write(*,"(x,A13,4x,F11.9,2x,A8)") "Actual error:", abs(Q1-exact), "(Normal)"
  write(*,"(x,A13,4x,F11.9,2x,A17)") "Actual error:", abs(Q2-exact), "(Clenshaw-Curtis)"
  write(*,"(x,A17,x,I6,2x,A8)") "Number of calls =", c, "(Normal)"
  write(*,"(x,A17,x,I3,2x,A17)") "Number of calls =", calls, "(Clenshaw-Curtis)"  

  write(*,*)  
  write(*,*) "----------------------------------------------------------------------"
  write(*,*)

  calls = 0  
  write(*,*) "Function = sin(x)/x"
  a = -1
  b = 1
  exact = 1.892166141
  write(*,"(x,A9,x,F4.1,x,A2,x,F3.1)") "Interval:", a, "to", b  
  Q1 = adapt(integ_eight,a,b,acc,eps,err)  
  write(*,"(x,A10,x,F11.9,2x,A8)") "Integral =", Q1, "(Normal)"
  c = calls
  calls = 0
  Q2 = adapt_clenshaw_curtis(integ_eight,a,b,acc,eps,err)
  write(*,"(x,A10,x,F11.9,2x,A17)") "Integral =", Q2, "(Clenshaw-Curtis)"  
  write(*,"(x,A10,x,F11.9)") "Should be:", exact
  write(*,"(x,A16,x,F11.9)") "Estimated error:", err
  write(*,"(x,A13,4x,F11.9,2x,A8)") "Actual error:", abs(Q1-exact), "(Normal)"
  write(*,"(x,A13,4x,F11.9,2x,A17)") "Actual error:", abs(Q2-exact), "(Clenshaw-Curtis)"
  write(*,"(x,A17,x,I6,2x,A8)") "Number of calls =", c, "(Normal)"
  write(*,"(x,A17,x,I6,2x,A17)") "Number of calls =", calls, "(Clenshaw-Curtis)"

  write(*,*)  
  write(*,*) "----------------------------------------------------------------------"
  write(*,*)

  calls = 0  
  write(*,*) "Function = 1/sqrt(x**2 + 1)"
  a = 0
  b = 1
  exact = 0.881373587
  write(*,"(x,A9,x,F4.1,x,A2,x,F3.1)") "Interval:", a, "to", b  
  Q1 = adapt(integ_nine,a,b,acc,eps,err)  
  write(*,"(x,A10,x,F11.9,2x,A8)") "Integral =", Q1, "(Normal)"
  c = calls
  calls = 0
  Q2 = adapt_clenshaw_curtis(integ_nine,a,b,acc,eps,err)
  write(*,"(x,A10,x,F11.9,2x,A17)") "Integral =", Q2, "(Clenshaw-Curtis)"  
  write(*,"(x,A10,x,F11.9)") "Should be:", exact
  write(*,"(x,A16,x,F11.9)") "Estimated error:", err
  write(*,"(x,A13,4x,F11.9,2x,A8)") "Actual error:", abs(Q1-exact), "(Normal)"
  write(*,"(x,A13,4x,F11.9,2x,A17)") "Actual error:", abs(Q2-exact), "(Clenshaw-Curtis)"
  write(*,"(x,A17,x,I6,2x,A8)") "Number of calls =", c, "(Normal)"
  write(*,"(x,A17,x,I6,2x,A17)") "Number of calls =", calls, "(Clenshaw-Curtis)"   
  
end program main
