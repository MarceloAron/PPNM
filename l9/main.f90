module functions
  implicit none

contains

  function integ_one(x) result(f)
    real*8 x, f
    f = log(x)/sqrt(x)
  end function integ_one

  function expect_value_hamil(alpha,x) result(f)
    real*8 x, f, alpha
    f = (alpha - alpha*alpha*x*x + x*x)*exp(-alpha*x*x)/2
  end function expect_value_hamil

  function norm(alpha,x) result(f)
    real*8 x, f, alpha
    f = exp(-alpha*x*x)
  end function norm  

end module functions

program main
  use integration
  use functions
  use ieee_arithmetic
  implicit none
  real*8 a, b, acc, eps, Q, exact, err, alpha, alpha_max, delta, Q1, Q2, E, mine, alpha_min

  acc = 1d-6
  eps = 1d-6

  write(*,*) "Function: ln(x)/sqrt(x)"
  a = 0
  b = 1
  exact = -4
  write(*,"(x,A9,x,F3.1,x,A2,x,F3.1)") "Interval:", a, "to", b
  Q = adapt(integ_one,a,b,acc,eps,err)
  write(*,"(x,A10,x,F12.9)") "Integral =", Q
  write(*,"(x,A10,x,F12.9)") "Should be:", exact
  write(*,"(x,A16,x,F11.9)") "Estimated error:", err
  write(*,"(x,A13,4x,F11.9)") "Actual error:", abs(Q-exact)

  write(*,*)
  write(*,*)

  write(*,*) "Function: <psi|H|psi>/<psi|psi>"
  write(*,*) "<psi|H|psi> = (alpha - alpha**2*x**2 + x**2)exp(-alpha*x**2)/2"
  write(*,*) "<psi|psi> = exp(-alpha*x**2)"
  a = ieee_value(a, ieee_negative_inf)
  b = ieee_value(a, ieee_positive_inf)
  alpha = 0.01
  alpha_max = 5
  delta = 0.01
  mine = 1000
  do while (alpha .le. alpha_max)
     Q1 = adapt_inf_par(alpha,expect_value_hamil,a,b,acc,eps,err)
     Q2 = adapt_inf_par(alpha,norm,a,b,acc,eps,err)     
     E = Q1/Q2
     write(0,*) alpha, E
     if (E .lt. mine) then
        mine = E
        alpha_min = alpha
     end if     
     alpha = alpha + delta
  end do
  
  write(*,"(x,A19,x,F4.2,x,A2,x,F4.2)") "Minimum value of E:", mine, "at", alpha_min
  write(*,*) "Results also shown in the form of a plot"
  
end program main
