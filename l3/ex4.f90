subroutine f(x)
  implicit none
  integer x

  if (x .eq. 0) then
     print*, "Zero"
  else if (x .eq. 1) then
     print*, "One" 
  else if (x .eq. 2) then
     print*, "Two"
  else if (x .eq. 3) then
     print*, "Three"
  else if (x .eq. 4) then
     print*, "Four"
  else if (x .eq. 5) then
     print*, "Five"
  else if (x .eq. 6) then
     print*, "Six"
  else if (x .eq. 7) then
     print*, "Seven"
  else if (x .eq. 8) then
     print*, "Eight"
  else if (x .eq. 9) then
     print*, "Nine"
  else
     print*, x,  "is not a digit"
  end if

end subroutine f
   
program ex4
  implicit none
  integer x, i
  real r

  do x=1,10
  print*, "x=",x
  call f(x)
end do

end program ex4
