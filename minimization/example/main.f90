program main
use multimin
implicit none
real*8::v(2),eps=1e-6
integer::steps

v=(/ -2,8 /)
print*,"MINIMIZATION OF ROSENBROCK'S FUNCTION"
print*,"start point      :",real(v)
print*,"value(start)     =",real(rosen(v))
steps= qnewton(rosen,v,eps)
print*,"steps            =",steps
print*,"minimum found at :",real(v)
print*,"value(minim)     =",real(rosen(v))
print*,"gradient at minim:",real(gradient(rosen,v))

v=(/ -2,8 /)
print*
print*,"MINIMIZATION OF HIMMELBLAU'S FUNCTION"
print*,"start point      :",real(v)
print*,"value(start)     =",real(himmel(v))
steps= qnewton(himmel,v,eps)
print*,"steps            =",steps
print*,"minimum found at :",real(v)
print*,"value(minim)     =",real(himmel(v))
print*,"gradient at minim:",real(gradient(himmel,v))

contains

function himmel(v) result(s)
real*8::v(:),s,x,y
x=v(1); y=v(2)
s=(x**2+y-11)**2+(x+y**2-7)**2
end function himmel

function rosen(v) result(s)
real*8::v(:),s,x,y
x=v(1); y=v(2)
s=(1-x)**2+100*(y-x*x)**2
end function rosen

end program main
