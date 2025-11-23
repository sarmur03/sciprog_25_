! Perform Matrix Multiplication C = A*B
function matmult(n,p,q,a,b) result(c)
	integer (kind = 4), intent(in) :: n, p, q
	real (kind = 8), intent(in):: a(n,p) , b(p,q)
        real (kind = 8) :: c(n,q)
        integer (kind = 4) :: i,j,k

! Initialise C
	
	c = 0.0

! Do matrix multiplication using C_ij = A_ik * B_kj
	do i = 1,n,1 ! Rows of A and C
		do j=1,q,1 ! Columns of B and C
			do k=1,p,1 ! Column of A and Row of B
				c(i,j) = c(i,j) + a(i,k)*b(k,j)
			end do
		end do 
	end do 
	return

end function matmult
