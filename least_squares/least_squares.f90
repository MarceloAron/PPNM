module least_squares
  use qr_givens
  use jacobi
  implicit none

contains

  function lsfit(m,f,x,y,dy,S) result(c)
    real*8, intent(in) :: x(:), y(:), dy(:)
    interface
       function f(i,z)
         integer, intent(in) :: i
         real*8, intent(in) :: z
         real*8 :: f
       end function f
    end interface
    integer, intent(in) :: m
    integer i, j, n
    real*8, intent(out) :: S(:,:)
    real*8 :: c(m), A(size(x),m), b(size(x)), Ai(m,m)

    n = size(x)

    do i=1,n
       b(i) = y(i)/dy(i)
       do j=1,m
          A(i,j) = f(j,x(i))/dy(i) 
       end do
    end do

    call qr_givens_decomp(A) 
    c = qr_givens_solve(A,b)
    Ai = qr_givens_inv(A)
    S = matmul(Ai,transpose(Ai))
  end function lsfit	

  function lsfit_svd(m,f,x,y,dy,Cov) result(c)    
    real*8, intent(in) :: x(:), y(:), dy(:)
     interface
       function f(i,z)
         integer, intent(in) :: i
         real*8, intent(in) :: z
         real*8 :: f
       end function f
    end interface
    integer, intent(in) :: m
    integer i, j, n, sweeps
    real*8, intent(out) :: Cov(:,:)
    real*8 :: c(m), ATA(m,m), A(size(x),m), V(m,m), b(size(x)), Ai(m,m), U(size(x),m), S(m,m), ATAi(m,m), Si(m,m)
    
    n = size(x)    

    do i=1,n
       b(i) = y(i)/dy(i)
       do j=1,m
          A(i,j) = f(j,x(i))/dy(i) 
       end do
    end do

    ATA=matmul(transpose(A),A)
    sweeps = evd_cyclic(ATA,V)
    S = sqrt(ATA)
    do i=1,m
       ATAi(i,i) = 1/ATA(i,i)
       Si(i,i) = 1/S(i,i)
    end do   
    U = matmul(matmul(A,V),sqrt(ATAi))
    c = matmul(matmul(V,Si),matmul(transpose(U),b))
    Cov = matmul(matmul(V,Si**2),transpose(V))
  end function lsfit_svd
  
	
end module least_squares
