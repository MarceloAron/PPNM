module jacobi
  implicit none

contains

  function evd_cyclic(A,V) result(sweeps)
    real*8 :: A(:,:), V(:,:), xp(size(A,1)), xq(size(A,1))
    real*8 :: theta, A1pp, A1qq, c, s
    integer i, n, p, q, changed, sweeps

    n = size(A,1)

    V(:,:) = 0

    do i=1,n
       V(i,i) = 1
    end do

    changed = 1
    sweeps = 0

    do while (changed .eq. 1)
       changed = 0
       sweeps = sweeps + 1
       do p=1,n
          do q=p+1,n
             theta = 0.5*atan2(2*A(p,q),A(q,q)-A(p,p))
             c = cos(theta)
             s = sin(theta)
             A1pp = c*c*A(p,p) - 2*s*c*A(p,q) + s*s*A(q,q)
             A1qq = s*s*A(p,p) + 2*s*c*A(p,q) + c*c*A(q,q)
             if (A1pp .ne. A(p,p) .or. A1qq .ne. A(q,q)) then
                changed = 1
                xp(:) = c*A(p,:) - s*A(q,:)
                xq(:) = s*A(p,:) + c*A(q,:)
                A(p,:) = xp(:)
                A(q,:) = xq(:)
                xp(:) = c*A(:,p) - s*A(:,q)
                xq(:) = s*A(:,p) + c*A(:,q)
                A(:,p) = xp(:)
                A(:,q) = xq(:)
                xp(:) = c*V(:,p) - s*V(:,q)
                xq(:) = s*V(:,p) + c*V(:,q)
                V(:,p) = xp(:)
                V(:,q) = xq(:)
             end if
          end do
       end do
    end do
  end function evd_cyclic

  function evd_low(A,V,m) result(sweeps)
    real*8 :: A(:,:), V(:,:), xp(size(A,1)), xq(size(A,1))
    real*8 :: theta, A1pp, A1qq, c, s
    integer i, n, p, q, changed, sweeps, m

    n = size(A,1)

    V(:,:) = 0

    do i=1,n
       V(i,i) = 1
    end do

    sweeps = 0

    do p=1,m
       changed = 1
       do while (changed .eq. 1)
          sweeps = sweeps + 1
          changed = 0
          do q=p+1,n
             theta = 0.5*atan2(2*A(p,q),A(q,q)-A(p,p))
             c = cos(theta)
             s = sin(theta)
             A1qq = s*s*A(p,p) + 2*s*c*A(p,q) + c*c*A(q,q)
             A1pp = c*c*A(p,p) - 2*s*c*A(p,q) + s*s*A(q,q)
             if (A1pp .ne. A(p,p) .or. A1qq .ne. A(q,q)) then
                changed = 1
                xp(:) = c*A(p,:) - s*A(q,:)
                xq(:) = s*A(p,:) + c*A(q,:)
                A(p,:) = xp(:)
                A(q,:) = xq(:)
                xp(:) = c*A(:,p) - s*A(:,q)
                xq(:) = s*A(:,p) + c*A(:,q)
                A(:,p) = xp(:)
                A(:,q) = xq(:)
                xp(:) = c*V(:,p) - s*V(:,q)
                xq(:) = s*V(:,p) + c*V(:,q)
                V(:,p) = xp(:)
                V(:,q) = xq(:)
             end if
          end do
       end do
    end do
  end function evd_low

   function evd_high(A,V,m) result(sweeps)
    real*8 :: A(:,:), V(:,:), xp(size(A,1)), xq(size(A,1))
    real*8 :: theta, A1pp, A1qq, c, s
    integer i, n, p, q, changed, sweeps, m

    n = size(A,1)

    V(:,:) = 0

    do i=1,n
       V(i,i) = 1
    end do

    sweeps = 0

    do p=1,m
       changed = 1
       do while (changed .eq. 1)
          sweeps = sweeps + 1
          changed = 0
          do q=p+1,n
             theta = 0.5*atan2(-2*A(p,q),A(p,p)-A(q,q))
             c = cos(theta)
             s = sin(theta)
             A1qq = s*s*A(p,p) + 2*s*c*A(p,q) + c*c*A(q,q)
             A1pp = c*c*A(p,p) - 2*s*c*A(p,q) + s*s*A(q,q)
             if (A1pp .ne. A(p,p) .or. A1qq .ne. A(q,q)) then
                changed = 1
                xp(:) = c*A(p,:) - s*A(q,:)
                xq(:) = s*A(p,:) + c*A(q,:)
                A(p,:) = xp(:)
                A(q,:) = xq(:)
                xp(:) = c*A(:,p) - s*A(:,q)
                xq(:) = s*A(:,p) + c*A(:,q)
                A(:,p) = xp(:)
                A(:,q) = xq(:)
                xp(:) = c*V(:,p) - s*V(:,q)
                xq(:) = s*V(:,p) + c*V(:,q)
                V(:,p) = xp(:)
                V(:,q) = xq(:)
             end if
          end do
       end do
    end do
  end function evd_high
  
  subroutine fmax(A,p,q)
    real*8, intent(in) :: A(:,:)
    real*8 :: maxi
    integer coord(2)
    integer, intent(out) :: p, q

    maxi = maxval(abs(A))
    coord = maxloc(abs(A))

5   if (coord(1) .eq. coord(2)) then
       coord = maxloc(abs(A), abs(A) .lt. abs(maxi))
       maxi = maxval(abs(A), abs(A) .lt. abs(maxi))
       go to 5
    else
       p = coord(1)
       q = coord(2)
    end if            
  end subroutine fmax  

  ! This is another function to find the coordinates of the highest
  ! off-diagonal element, but it was written by Dmitri Fedorov
  function find_max_off_index(A) result(pq)    
    real*8::A(:,:), largest    
    integer::pq(2), p, q    
    pq(1) = 1    
    pq(2) = 2    
    largest = abs(A(pq(1),pq(2)))    
    do p=1,size(A,1)       
       do q=p+1,size(A,1)          
          if(abs(A(p,q)) .gt. largest) then             
             largest = abs(A(p,q))             
             pq(1) = p             
             pq(2) = q             
          end if          
       end do       
    end do  
end function
  
  function evd_classic(A,V) result(sweeps)
    real*8 :: A(:,:), V(:,:), xp(size(A,1)), xq(size(A,1))
    real*8 :: theta, A1pp, A1qq, c, s
    integer i, n, p, q, changed, pq(2), sweeps

    n = size(A,1)

    V(:,:) = 0

    do i=1,n
       V(i,i) = 1
    end do

    changed = 1
    sweeps = 0    

    do while (changed .eq. 1)
       changed = 0
       sweeps = sweeps + 1
!       pq=find_max_off_index(A) !Add those three lines and comment the "call"
!       p=pq(1)                  !line to use the function created by the 
!       q=pq(2)                  !professor instead of mine  
       call fmax(A,p,q)
       theta = 0.5*atan2(2*A(p,q),A(q,q)-A(p,p))       
       c = cos(theta)       
       s = sin(theta)      
       A1pp = c*c*A(p,p) - 2*s*c*A(p,q) + s*s*A(q,q)       
       A1qq = s*s*A(p,p) + 2*s*c*A(p,q) + c*c*A(q,q)      
       if (A1pp .ne. A(p,p) .or. A1qq .ne. A(q,q)) then          
          changed = 1     
          xp(:) = c*A(p,:) - s*A(q,:)          
          xq(:) = s*A(p,:) + c*A(q,:)          
          A(p,:) = xp(:)          
          A(q,:) = xq(:)          
          xp(:) = c*A(:,p) - s*A(:,q)          
          xq(:) = s*A(:,p) + c*A(:,q)          
          A(:,p) = xp(:)          
          A(:,q) = xq(:)          
          xp(:) = c*V(:,p) - s*V(:,q)          
          xq(:) = s*V(:,p) + c*V(:,q)         
          V(:,p) = xp(:)          
          V(:,q) = xq(:)          
       end if
    end do    
  end function evd_classic

  subroutine matrix_print(A)    
    real*8 :: A(:,:)
    integer i, j

    do i=1,size(A,1)
       write(*,"(80F8.3)") (A(i,j), j=1,size(A,2))
    end do
  end subroutine matrix_print  

end module jacobi
