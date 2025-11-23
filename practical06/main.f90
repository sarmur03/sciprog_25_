! This is a program to calculate the resultant matrix from multiplying matrix A and B, where the elements of A are 
! the sum of the value of the indices (i+j) and the elements of B are the differences (i-j).

! Function Prototype
module functions
	interface
		function matmult(n,p,q,a,b) result(c)
			integer (kind = 4), intent(in) :: n, p, q
			real (kind = 8), intent(in):: a(n,p) , b(p,q)  
			real (kind = 8) :: c(n,q)
			integer (kind = 4) :: i,j,k
		end function matmult
	end interface
end module functions



program matrixmult
	use functions
	implicit none
	integer (kind=4), parameter :: n=5, p=3, q=4
	real (kind=8) :: a(n,p) , b(p,q) , c(n,q)
	integer(kind=4) :: i,j,k

! Initialise A

	do i = 1,n,1
		do j = 1,p,1
			a(i,j) = i+j
		end do 
	end do 
	
! Initialise B
	do i = 1,p,1
		do j=1,q,1
			b(i,j) = i-j
		end do 
	end do

! Initialise C. Shortcut below sets entire matrix to zero
	c = 0.0

! Perform Matrix Multiplication 

	c = matmult(n,p,q,a,b)

! Print Matrix A
	write(6,"(/,a,/,/)") "This is matrix A: "
		do i=1,n,1
			do j=1,p,1
				write(6,"(f3.0)", advance = "no") a(i,j)
			end do
			write(6,*)
		end do



! Print Matrix B
        write(6,"(/,a,/,/)") "This is matrix B: "
                do i=1,p,1
                        do j=1,q,1
                                write(6,"(f3.0)", advance = "no") b(i,j)
                        end do
                        write(6,*)
                end do

! Print Matrix C
        write(6,"(/,a,/,/)") "This is matrix C: "
                do i=1,n,1
                        do j=1,q,1
                                write(6,"(f4.0)", advance = "no") c(i,j)
                        end do
                        write(6,*)
                end do

end program matrixmult
