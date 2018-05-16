module funs
implicit none
integer,parameter::n=2
integer::ncalls
contains

function grosen(x) result(g)
real*8::x(:),g(size(x))
ncalls=ncalls+1
g(1)=2*(1-x(1))*(-1)+100*2*(x(2)-x(1)**2)*(-1)*2*x(1)
g(2)=100*2*(x(2)-x(1)**2)
end function

function ghimmel(x) result(g)
real*8::x(:),g(size(x))
ncalls=ncalls+1
g(1)=2*(x(1)**2+x(2)-11)*2*x(1)+2*(x(1)+x(2)**2-7)
g(2)=2*(x(1)**2+x(2)-11)       +2*(x(1)+x(2)**2-7)*2*x(2)
end function

end module funs

program main
use roots; use funs
implicit none
real*8::x(n),dx=1d-6,eps=1d-3

x=(/-2,8/)
print*,"FINDING THE ROOT OF THE GRADIENT OF THE ROSENBROCK FUNCTION:"
print*
print*,"dx=",dx,"eps=",eps
print*,"START POINT:",x
ncalls=0
 call newton(grosen,x,dx,eps)
print*,"ncalls=",ncalls
print*,"ROOT       :",x
print*,"   gradient:",grosen(x)

print*
print*,"FINDING ROOTS OF THE GRADIENT OF THE HIMMELBLAU FUNCTION:"
print*,"dx=",dx,"eps=",eps

print*
x=(/4,3/)
print*,"START POINT:",x
ncalls=0
 call newton(ghimmel,x,dx,eps)
print*,"ncalls=",ncalls      
print*,"ROOT       :",x
print*,"   gradient:",ghimmel(x)

print*
x=(/-2,3/)
print*,"START POINT:",x
ncalls=0
call newton(ghimmel,x,dx,eps)
print*,"ncalls=",ncalls
print*,"ROOT       :",x
print*,"   gradient:",ghimmel(x)

print*
x=(/-4,-3/)
print*,"START POINT:",x
ncalls=0
call newton(ghimmel,x,dx,eps)
print*,"ncalls=",ncalls
print*,"ROOT       :",x
print*,"   gradient:",ghimmel(x)

print*
x=(/4,-2/)
print*,"START POINT:",x
ncalls=0
call newton(ghimmel,x,dx,eps)
print*,"ncalls=",ncalls
print*,"ROOT       :",x
print*,"   gradient:",ghimmel(x)

print*
x=(/1,1/)
print*,"START POINT:",x
ncalls=0
call newton(ghimmel,x,dx,eps)
print*,"ncalls=",ncalls
print*,"ROOT       :",x
print*,"   gradient:",ghimmel(x)

end program main
