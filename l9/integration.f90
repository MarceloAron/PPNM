module integration
  use ieee_arithmetic  
  implicit none  

contains

  recursive function adapt24(f,a,b,acc,eps,f2,f3,nrec,err) result(Q)
    real*8 a, b, acc, eps, f2, f3
    integer nrec
    interface
       function f(x)
         real*8 x, f
       end function f       
    end interface
    real*8 f1, f4, Q, r, tol, err, Q1, Q2

    do while (nrec .lt. 1000000) !Maximum number of recursions
       f1 = f(a+(b-a)/6) !Closed set nodes, this is i=2 and n=7
       f4 = f(a+5*(b-a)/6) !Closed set nodes, this is i=6 and n=7
       Q = (2*f1 + f2 + f3 + 2*f4)*(b-a)/6 !Higher order quadrature integration
       r = (f1 + f2 + f3 + f4)*(b-a)/4 !Lower order quadrature integration
       tol = acc + eps*abs(Q) !Defining the tolerance
       err = abs(Q-r)/2 !Defining the estimate of the integration error

!      write(0,*) "Q=", real(Q), "r=", real(r), "err=", real(err), "tol=", real(tol), "nrec=", nrec
       if (err .lt. tol) then
!         write(0,*) "Accepted" !Test to see if this was working
          return !THIS IS WHAT STOPS THE PROGRAM (we have "Accepted" multiple times because of recursion.
       else
          !Subdividing the interval into two half integrals, calling the function recursively
          Q1 = adapt24(f,a,(a+b)/2,acc/sqrt(2.),eps,f1,f2,nrec+1,err) 
          Q2 = adapt24(f,(a+b)/2,b,acc/sqrt(2.),eps,f3,f4,nrec+1,err)
          Q = Q1 + Q2
          return
       end if
    end do  
  end function adapt24  
  
  function adapt(f,a,b,acc,eps,err) result(Q)
    real*8 a, b, acc, eps, Q, err
    interface
       function f(x)
         real*8 x, f
       end function f
    end interface
    real*8 f2, f3
    integer nrec

    nrec = 0
    f2 = f(a+2*(b-a)/6) !Closed set nodes, this is i=3 and n=7
    f3 = f(a+4*(b-a)/6) !Closed set nodes, this is i=5 and n=7 (i=4 is skipped for some reason)
    Q = adapt24(f,a,b,acc,eps,f2,f3,nrec,err)
  end function adapt

  !Variable transformations reducing infinite intervals to finite intervals
  function f_inf_inf(f,t)
    interface       
       function f(x)                 
         real*8 x, f         
       end function f       
    end interface    
    real*8 t, f_inf_inf
    f_inf_inf = f(t/(1-t*t))*(1+t*t)/(1-t*t)**2    
  end function f_inf_inf  

  function f_fi_inf(a,f,t)
    interface
       function f(x)         
         real*8 x, f         
   end function f
    end interface    
    real*8 a, t, f_fi_inf
    f_fi_inf = f(a + (1-t)/t)/(t*t)    
  end function f_fi_inf  

  function f_inf_fi(b,f,t)    
    interface       
       function f(x)         
         real*8 x, f         
       end function f       
    end interface    
    real*8 b, t, f_inf_fi    
    f_inf_fi = f(b + t/(1+t))/(1+t)**2
  end function f_inf_fi  

  !Generalization of the integrator to infinite limits
  function adapt_inf(f,a,b,acc,eps,err) result(Q)
    real*8 a, b, acc, eps, Q, err
    interface
       function f(x)
         real*8 x, f
       end function f
    end interface
    real*8 f2, f3, a_new, b_new
    integer nrec

!    write(0,*) "This is where I am 0"    
    if (ieee_is_finite(a) .eqv. .FALSE.) then       
       if (ieee_is_finite(b) .eqv. .FALSE.) then !Both a and b are infinite
!          write(0,*) "This is where I am 1"
          a_new = -1          
          b_new = 1          
          nrec = 0         
          f2 = f_inf_inf(f,a_new + 2*(b_new-a_new)/6)          
          f3 = f_inf_inf(f,a_new + 4*(a_new-b_new)/6)          
          Q = adapt24_inf(f_inf_inf,f,a_new,b_new,acc,eps,f2,f3,nrec,err)          
       else if (ieee_is_finite(b) .eqv. .TRUE.) then !Only a is infinite
!          write(0,*) "This is where I am 2"
          a_new = -1          
          b_new = 0        
          nrec = 0          
          f2 = f_inf_fi(b,f,a_new + 2*(b_new-a_new)/6)          
          f3 = f_inf_fi(b,f,a_new + 4*(b_new-a_new)/6)          
          Q = adapt24_inf_half(b,f_inf_fi,f,a_new,b_new,acc,eps,f2,f3,nrec,err)          
       end if       
    else if (ieee_is_finite(a) .eqv. .TRUE.) then       
       if (ieee_is_finite(b) .eqv. .FALSE.) then !Only b is infinite          
!          write(0,*) "This is where I am 3"
          a_new = 0          
          b_new = 1          
          nrec = 0          
          f2 = f_fi_inf(a,f,a_new + 2*(b_new-a_new)/6)          
          f3 = f_fi_inf(a,f,a_new + 4*(b_new-a_new)/6)          
          Q = adapt24_inf_half(a,f_fi_inf,f,a_new,b_new,acc,eps,f2,f3,nrec,err)          
       else if (ieee_is_finite(b) .eqv. .TRUE.) then !Both a and b are finite          
!          write(0,*) "This is where I am 4"
          nrec = 0          
          f2 = f(a+2*(b-a)/6)          
          f3 = f(a+4*(b-a)/6)          
          Q = adapt24(f,a,b,acc,eps,f2,f3,nrec,err)          
       end if       
    end if    
  end function adapt_inf  

    recursive function adapt24_inf(g,f,a,b,acc,eps,f2,f3,nrec,err) result(Q)
    real*8 a, b, acc, eps, f2, f3
    integer nrec
    interface
       function g(f,t)
         real*8 t, g
         interface
            function f(x)
              real*8 x, f
            end function f
         end interface         
       end function g     
       function f(x)
         real*8 x, f
       end function f       
    end interface
    real*8 f1, f4, Q, r, tol, err, Q1, Q2

    do while (nrec .lt. 1000000) !Maximum number of recursions
       f1 = g(f,a+(b-a)/6) !Closed set nodes, this is i=2 and n=7
       f4 = g(f,a+5*(b-a)/6) !Closed set nodes, this is i=6 and n=7
       Q = (2*f1 + f2 + f3 + 2*f4)*(b-a)/6 !Higher order quadrature integration
       r = (f1 + f2 + f3 + f4)*(b-a)/4 !Lower order quadrature integration
       tol = acc + eps*abs(Q) !Defining the tolerance
       err = abs(Q-r)/2 !Defining the estimate of the integration error

!       write(0,*) "Q=", real(Q), "r=", real(r), "err=", real(err), "tol=", real(tol), "nrec=", nrec
       if (err .lt. tol) then
!          write(0,*) "Accepted"
          return !THIS IS WHAT STOPS THE PROGRAM (we have "Accepted" multiple times because of recursion.
       else
          !Subdividing the interval into two half integrals, calling the function recursively
          Q1 = adapt24_inf(g,f,a,(a+b)/2,acc/sqrt(2.),eps,f1,f2,nrec+1,err) 
          Q2 = adapt24_inf(g,f,(a+b)/2,b,acc/sqrt(2.),eps,f3,f4,nrec+1,err)
          Q = Q1 + Q2
          return
       end if
    end do  
  end function adapt24_inf

   recursive function adapt24_inf_half(ab_old,g,f,a,b,acc,eps,f2,f3,nrec,err) result(Q)
    real*8 a, b, acc, eps, f2, f3, ab_old
    integer nrec
    interface
       function g(a,f,t)
         real*8 a, t, g
         interface
            function f(x)
              real*8 x, f
            end function f
         end interface         
       end function g
       function f(x)
         real*8 x, f
       end function f       
    end interface
    real*8 f1, f4, Q, r, tol, err, Q1, Q2

    do while (nrec .lt. 1000000) !Maximum number of recursions
       f1 = g(ab_old,f,a+(b-a)/6) !Closed set nodes, this is i=2 and n=7
       f4 = g(ab_old,f,a+5*(b-a)/6) !Closed set nodes, this is i=6 and n=7
       Q = (2*f1 + f2 + f3 + 2*f4)*(b-a)/6 !Higher order quadrature integration
       r = (f1 + f2 + f3 + f4)*(b-a)/4 !Lower order quadrature integration
       tol = acc + eps*abs(Q) !Defining the tolerance
       err = abs(Q-r)/2 !Defining the estimate of the integration error

!       write(0,*) "Q=", real(Q), "r=", real(r), "err=", real(err), "tol=", real(tol), "nrec=", nrec
       if (err .lt. tol) then
!          write(0,*) "Accepted"
          return !THIS IS WHAT STOPS THE PROGRAM (we have "Accepted" multiple times because of recursion).
       else
          !Subdividing the interval into two half integrals, calling the function recursively
          Q1 = adapt24_inf_half(ab_old,g,f,a,(a+b)/2,acc/sqrt(2.),eps,f1,f2,nrec+1,err) 
          Q2 = adapt24_inf_half(ab_old,g,f,(a+b)/2,b,acc/sqrt(2.),eps,f3,f4,nrec+1,err)
          Q = Q1 + Q2
          return
       end if
    end do  
  end function adapt24_inf_half

  function f_clenshaw_curtis(a,b,f,t)
    interface
       function f(x)
         real*8 x, f
       end function f
    end interface
    real*8 t, f_clenshaw_curtis, a, b
    if (a .eq. -1 .and. b .eq. 1) then       
       f_clenshaw_curtis = f(cos(t))*sin(t) !For interval from -1 to 1
    else if (a .eq. 0 .and. b .eq. 1) then       
       f_clenshaw_curtis = f(0.5*(a+b)+0.5*(a-b)*cos(t))*sin(t)*0.5*(b-a) !From interval from 0 to 1
    end if    
  end function f_clenshaw_curtis

  function adapt_clenshaw_curtis(f,a,b,acc,eps,err) result(Q)
    real*8 a, b, acc, eps, err, Q
    interface
       function f(x)
         real*8 x, f
       end function f
    end interface
    real*8 f2, f3, a_new, b_new, x
    integer nrec

    nrec = 0
    a_new = 0
    b_new = 3.141592653589793d0
    f2 = f_clenshaw_curtis(a,b,f,a_new + 2*(b_new-a_new)/6)    
    f3 = f_clenshaw_curtis(a,b,f,a_new + 4*(a_new-b_new)/6)    
    Q = adapt24_clenshaw_curtis(a,b,f_clenshaw_curtis,f,a_new,b_new,acc,eps,f2,f3,nrec,err)
  end function adapt_clenshaw_curtis

   recursive function adapt24_clenshaw_curtis(a_old,b_old,g,f,a,b,acc,eps,f2,f3,nrec,err) result(Q)
    real*8 a, b, acc, eps, f2, f3, a_old, b_old
    integer nrec
    interface
       function g(a,b,f,t)
         real*8 a, b, t, g
         interface
            function f(x)
              real*8 x, f
            end function f
         end interface         
       end function g
       function f(x)
         real*8 x, f
       end function f       
    end interface
    real*8 f1, f4, Q, r, tol, err, Q1, Q2

    do while (nrec .lt. 1000000) !Maximum number of recursions
       f1 = g(a_old,b_old,f,a+(b-a)/6) !Closed set nodes, this is i=2 and n=7
       f4 = g(a_old,b_old,f,a+5*(b-a)/6) !Closed set nodes, this is i=6 and n=7
       Q = (2*f1 + f2 + f3 + 2*f4)*(b-a)/6 !Higher order quadrature integration
       r = (f1 + f2 + f3 + f4)*(b-a)/4 !Lower order quadrature integration
       tol = acc + eps*abs(Q) !Defining the tolerance
       err = abs(Q-r)/2 !Defining the estimate of the integration error

!      write(0,*) "Q=", real(Q), "r=", real(r), "err=", real(err), "tol=", real(tol), "nrec=", nrec
       if (err .lt. tol) then
!         write(0,*) "Accepted"
          return !THIS IS WHAT STOPS THE PROGRAM (we have "Accepted" multiple times because of recursion).
       else
          !Subdividing the interval into two half integrals, calling the function recursively
          Q1 = adapt24_clenshaw_curtis(a_old,b_old,g,f,a,(a+b)/2,acc/sqrt(2.),eps,f1,f2,nrec+1,err) 
          Q2 = adapt24_clenshaw_curtis(a_old,b_old,g,f,(a+b)/2,b,acc/sqrt(2.),eps,f3,f4,nrec+1,err)
          Q = Q1 + Q2
          return
       end if
    end do  
  end function adapt24_clenshaw_curtis

   function f_inf_inf_par(alpha,f,t)
    interface       
       function f(alpha,x)                 
         real*8 x, f, alpha         
       end function f       
    end interface    
    real*8 t, f_inf_inf_par, alpha
    f_inf_inf_par = f(alpha,t/(1-t*t))*(1+t*t)/(1-t*t)**2    
  end function f_inf_inf_par  

  function adapt_inf_par(alpha,f,a,b,acc,eps,err) result(Q)    
    real*8 a, b, acc, eps, Q, err, alpha
    interface       
       function f(alpha,x)
         real*8 x, f, alpha
       end function f
    end interface
    real*8 f2, f3, a_new, b_new
    integer nrec

    !I will write only for both limits infinite because that's the
    !case we are interested in, no need to worry about the others
    !now, but the idea is the same.
    a_new = -1    
    b_new = 1    
    nrec = 0    
    f2 = f_inf_inf_par(alpha,f,a_new + 2*(b_new-a_new)/6)    
    f3 = f_inf_inf_par(alpha,f,a_new + 4*(a_new-b_new)/6)    
    Q = adapt24_inf_par(alpha,f_inf_inf_par,f,a_new,b_new,acc,eps,f2,f3,nrec,err)    
  end function adapt_inf_par  

    recursive function adapt24_inf_par(alpha,g,f,a,b,acc,eps,f2,f3,nrec,err) result(Q)
    real*8 a, b, acc, eps, f2, f3, alpha
    integer nrec
    interface
       function g(alpha,f,t)
         real*8 t, g, alpha
         interface
            function f(alpha,x)
              real*8 x, f, alpha
            end function f
         end interface         
       end function g     
       function f(alpha,x)
         real*8 x, f, alpha
       end function f       
    end interface
    real*8 f1, f4, Q, r, tol, err, Q1, Q2

    do while (nrec .lt. 1000000) !Maximum number of recursions
       f1 = g(alpha,f,a+(b-a)/6) !Closed set nodes, this is i=2 and n=7
       f4 = g(alpha,f,a+5*(b-a)/6) !Closed set nodes, this is i=6 and n=7
       Q = (2*f1 + f2 + f3 + 2*f4)*(b-a)/6 !Higher order quadrature integration
       r = (f1 + f2 + f3 + f4)*(b-a)/4 !Lower order quadrature integration
       tol = acc + eps*abs(Q) !Defining the tolerance
       err = abs(Q-r)/2 !Defining the estimate of the integration error

!       write(0,*) "Q=", real(Q), "r=", real(r), "err=", real(err), "tol=", real(tol), "nrec=", nrec
       if (err .lt. tol) then
!          write(0,*) "Accepted"
          return !THIS IS WHAT STOPS THE PROGRAM (we have "Accepted" multiple times because of recursion.
       else
          !Subdividing the interval into two half integrals, calling the function recursively
          Q1 = adapt24_inf_par(alpha,g,f,a,(a+b)/2,acc/sqrt(2.),eps,f1,f2,nrec+1,err) 
          Q2 = adapt24_inf_par(alpha,g,f,(a+b)/2,b,acc/sqrt(2.),eps,f3,f4,nrec+1,err)
          Q = Q1 + Q2
          return
       end if
    end do  
  end function adapt24_inf_par  

end module integration

    
    
          
       
         
