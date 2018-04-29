module qr_givens
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

  subroutine qr_givens_decomp(A)
    real*8 :: A(:,:)
    real*8 :: theta, s, c, xq, xp
    integer k, m, n, p, q

    n = size(A,1)
    m = size(A,2)

    do q=1,m
       do p=q+1,n
          theta = atan2(A(p,q),A(q,q))
          do k=q,m
             xq = A(q,k)
             xp = A(p,k)
             A(p,k) = cos(theta)*xp - sin(theta)*xq
             A(q,k) = sin(theta)*xp + cos(theta)*xq
          end do
          A(p,q) = theta
       end do
    end do
  end subroutine qr_givens_decomp

  subroutine qr_givens_getr(A,R)
    real*8 :: A(:,:), R(:,:)
    integer i, j

    R(:,:) = 0
    do i=1,size(A,2)
       do j=i,size(A,2)
          R(i,j) = A(i,j)
       end do
    end do
  end subroutine qr_givens_getr

  subroutine qr_givens_getq(A,Q)
    real*8 :: A(:,:), Q(:,:)
    real*8, allocatable :: e(:)
    integer i,k

    allocate(e(size(A,1)))

    do i=1,size(A,1)
       e(:) = 0
       e(i) = 1
       call qrqt(A,e)
       Q(i,:) = e(:)
    end do
  end subroutine qr_givens_getq

  subroutine qr_givens_back(A,x)
    real*8 :: A(:,:), x(:)
    real*8 :: s
    integer i, k, m

    m = size(A,2)

    do i=m,1,-1
       s = 0
       do k=i+1,m
          s = s + A(i,k)*x(k)
       end do
       x(i) = (x(i) - s)/A(i,i)
    end do
  end subroutine qr_givens_back

  subroutine qrqt(A,v)
    real*8 :: A(:,:), v(:)
    real*8 :: vp, vq, theta
    integer m, n, p, q

    n = size(A,1)
    m = size(A,2)

    do q=1,m
       do p=q+1,n
          theta = A(p,q)
          vp = v(p)
          vq = v(q)
          v(p) = cos(theta)*vp - sin(theta)*vq
          v(q) = sin(theta)*vp + cos(theta)*vq
       end do
    end do
  end subroutine qrqt

  function qr_givens_solve(A,b) result(x)
    real*8 :: A(:,:), b(:)
    real*8, allocatable :: v(:), x(:)
    integer i, m, n

    n = size(A,1)
    m = size(A,2)
    allocate(v(n),x(m))
    v(:) = b(:)
    call qrqt(A,v)
    x(1:m) = v(1:m)
    call qr_givens_back(A,x)
  end function qr_givens_solve

  function qr_givens_inv(A) result(B)
    real*8 A(:,:), B(size(A,2),size(A,2))
    real*8 e(size(A,1))
    integer i
    e(:) = 0
    do i=1,size(A,2)
       e(i) = 1
       B(:,i) = qr_givens_solve(A,e)
       e(i) = 0
    end do
  end function qr_givens_inv

end module qr_givens


    
