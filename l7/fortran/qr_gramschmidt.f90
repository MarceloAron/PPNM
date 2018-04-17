module qr_gramschmidt
  implicit none

contains

  subroutine matrix_print(A)
    real*8 :: A(:,:)
    integer i, j

    do i=1,size(A,1)
       write(*,"(80F8.3)") (A(i,j), j=1,size(A,2))
    end do
  end subroutine matrix_print

  
  subroutine vector_print(v)
    real*8 :: v(:)
    integer i

    do i=1,size(v)
       write(*,"(80F8.3)") v(i)
    end do
  end subroutine vector_print  

  subroutine qr_decomp(A,R)
    real*8 :: A(:,:), R(:,:)
    integer i, j, m, n

    n = size(A,1)
    m = size(A,2)

    do i=1,m
       R(i,i) = norm2(A(:,i))
       A(:,i) = A(:,i)/R(i,i)
       do j=i+1,m
          R(i,j) = dot_product(A(:,i),A(:,j))
          A(:,j) = A(:,j) - A(:,i)*R(i,j)
       end do
    end do   
  end subroutine qr_decomp

  subroutine qr_solve(R,x,y)
    real*8 :: R(:,:)
    real*8 :: x(:), y(:)
    integer i, j, n

    n = size(x)

    x(n) = y(n)/R(n,n)
    
    do i=n-1,1,-1
       do j=i+1,n
          x(i) = x(i) + R(i,j)*x(j)
       end do
       x(i) = (y(i) - x(i))/R(i,i) 
    end do
  end subroutine qr_solve
       
  subroutine matrix_inverse(A,R,B)
    real*8 :: A(:,:), R(:,:), B(:,:)
    real*8, allocatable :: Id(:,:)
    integer i, j, n

    n = size(A,2)
    allocate(Id(n,n))

    do i=1,n
       do j=1,n
          if (i .eq. j) then
             Id(i,j) = 1
          else
             Id(i,j) = 0
          end if
       end do
    end do

    write(*,*) "Matrix I="
    call matrix_print(Id)

    do i=1,n
       call qr_solve(R,B(:,i),matmul(transpose(A),Id(:,i)))
    end do
  end subroutine matrix_inverse
    

end module qr_gramschmidt
