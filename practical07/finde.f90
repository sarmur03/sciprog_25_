!Find e using Taylor series expansion from e^x

program finde
	implicit none
    	interface
		function factorial(n)
            		integer (kind=4) :: factorial, n
        	end function
    	end interface
    	integer (kind=4) :: i, order, ierr
    	real (kind=8) :: e
    	real (kind=8), allocatable :: terms(:)

!Enter polynomial order
    	write(6, *) 'Enter the required polynomial order'
    	read(5, *, iostat=ierr) order
    	
! If the order is not given as an integer stop the program
	if(ierr .ne. 0) then 
		write(6,*) ' Problem with input '
		stop 
	end if 
    	allocate(terms(order))


! Output the approximation of e at each step of the calculation

    	do i=1, order
        	terms(i)=1.0/real(factorial(i),kind=8)
        	write(6,'(a,i2,a,f20.16)') "e term for order ", i, " is ", terms(i)
    	end do

    	e=dble(1.0)
    	write(6,*) "e is estimated as ", e+sum(terms), "/" ,"The error in our approximation is" ,e+sum(terms)-dexp(e)

	deallocate(terms)
    
	stop

end program finde


integer (kind=4) function factorial(n)

    	implicit none
    	integer (kind=4), intent(in) :: n
    	integer (kind=4) :: i, x

   	x=1
    	do i=1, n
        	x=x*i
    	end do

    	factorial = x

end function factorial


