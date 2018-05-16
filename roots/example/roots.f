module roots
use qr_givens
implicit none
contains
subroutine newton(f,x,dx,eps)
real*8::x(:),dx,eps
interface
function f(x)
real*8::x(:),f(size(x))
end function
end interface
real*8,allocatable::fx(:),df(:),jac(:,:),s(:),z(:),fz(:),lambda
integer::i,j,n
n=size(x)
allocate(jac(n,n))
do while (.true.)
	fx=f(x)
	do j=1,n
		x(j)=x(j)+dx
		df=f(x)-fx
		do i=1,n
			jac(i,j)=df(i)/dx
		end do
		x(j)=x(j)-dx
	end do
	call qr_givens_decomp(jac)
	s=qr_givens_solve(jac,-fx)
	lambda=1
	do while (.true.)
		z=x+lambda*s
		fz=f(z)
		if ( norm2(fz)<(1-lambda/2)*norm2(fx) .or. lambda<1.0/64) then
			exit
		end if
		lambda=lambda/2
	end do
	x=z
	fx=fz
	if ( norm2(s)<dx .or. norm2(fx)<eps ) then
		exit
	end if
end do
end subroutine newton
end module roots
