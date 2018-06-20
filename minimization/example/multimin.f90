module multimin
implicit none
!real*8,parameter::dx=1d0/1048576
real*8,parameter::dx=1d0/67108864
contains

subroutine set_identity(m)
real*8  m(:,:)
integer i
m=0
do i=1,size(m,1)
	m(i,i)=1
end do
end subroutine

subroutine update(m,beta,u,v)
real*8::u(:),v(:),beta,m(size(u),size(v))
integer i,j
do i=1,size(u)
do j=1,size(v)
	m(i,j)=m(i,j)+beta*u(i)*v(j)
end do
end do
end subroutine update

function gradient(s,x) result(g)
interface
function s(x)
real*8::s,x(:)
end function
end interface
real*8::x(:),sx,g(size(x)),xi,h
integer i
sx=s(x)
do i=1,size(x)
	xi=x(i)
	h=abs(xi)*dx
	x(i)=xi+h
	g(i)=(s(x)-sx)/h
	x(i)=xi
end do
end function gradient

function qnewton(s,x,eps) result(nsteps)
real*8::x(:),eps
interface
function s(x)
real*8::s,x(:)
end function
end interface
real*8::b(size(x),size(x)),gx(size(x)),gz(size(x)),step(size(x)),z(size(x)),u(size(x)),y(size(x))
real*8::sx,sz,lambda,gam,uy,sy,a(size(x))
integer::i,n,nsteps
n=size(x); nsteps=0
call set_identity(b)
gx=gradient(s,x)
sx=s(x)
do while(.true.)   
   nsteps=nsteps+1   
   step=-matmul(b,gx)   
   if(norm2(step)<dx)then
      write(6,*)"qnewton: converged: norm(step)=",real(norm2(step))," < dx=",real(dx)      
      exit      
   end if   
   if(norm2(gx).lt.eps)then      
      write(6,*)"qnewton: converged: norm(grad)=",real(norm2(gx))," < eps=",real(eps)      
      exit      
   end if   
   lambda=1   
   do while(.true.)          
    z=x+lambda*step    
    sz=s(z)
    if( sz<sx+0.02*dot_product(step,gx) )then       
       exit       
    end if    
    lambda=lambda/2    
    if(lambda<1.d0/64)then       
!       call set_identity(b)       
       exit       
    end if    
 end do 
 gz=gradient(s,z) 
 y=gz-gx 
 u=step-matmul(b,y) 
 uy=dot_product(u,y) 
 sy=dot_product(step,y) 
 if(sy>1e-12)then    
    gam=uy/2/sy    
    a=u-gam*step
    call update(b,1/sy,a,step);    
    call update(b,1/sy,step,a);    
 end if 
 x=z; sx=sz; gx=gz 
end do
end function qnewton

end module multimin
