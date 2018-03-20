program ex
  use module_nvector
  implicit none
  integer n, i
  real r, g, val, d
  type (nvector) :: v
  type (nvector) :: u
  type (nvector) :: a
  type (nvector) :: b
  
  n = 5

  call nvector_alloc(v,n)
  print*, "Allocated:", allocated(v%data), "Size of v:", size(v%data)
  call nvector_alloc(u,n)
  print*, "Allocated:", allocated(u%data), "Size of u:", size(u%data)

  do i=1,n
  call random_number(r)
  val = int(10*r)
  call nvector_set(v,i,val)
  g = nvector_get(v,i)
  if (g == val) then
     print*, "OK"
  else    
     print*, "BUG"
  end if  
end do

 do i=1,n
  call random_number(r)
  val = int(10*r)
  call nvector_set(u,i,val)
  g = nvector_get(u,i)
  if (g == val) then
     print*, "OK"
  else    
     print*, "BUG"
  end if  
end do

print*, "v%data is:", v%data
print*, "u%data is:", u%data

d = nvector_dot_product(v,u)

print*, "The dot product of v and u is:", d

a = nvector_add(n,v,u)
b = nvector_sub(n,v,u)

print*, "The sum of v and u is:", a%data
print*, "The subtraction of v by u is:", b%data

print*, "Now we will test the nvector_set_zero:"

call nvector_set_zero(v)
call nvector_set_zero(u)

print*, v%data
print*, u%data

print*, "Now we will test the nvector_dealloc:"

  call nvector_dealloc(v)
  print*, "Allocated:", allocated(v%data)
  call nvector_dealloc(u)
  print*, "Allocated:", allocated(u%data)

end program ex
