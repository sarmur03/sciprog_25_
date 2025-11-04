

module fib
	contains
	subroutine fibonacci(a,b)
		integer(kind=4), intent(inout) :: a,b
		integer(kind = 4) :: next
!a = Fn-1, b=Fn-2
		next = a+b
		b=a; a = next
	end subroutine fibonacci
end module

program fibonacci_prog
	use fib
	implicit none
	integer(kind=4) :: n,i,f1=1,f0=0,ios

! Get user to enter a number
	write(6,*) "Enter a positive integer"
	read(5,*,IOSTAT=ios) n

! Check that user entered an integer and not a different variable type
	if (ios /= 0) then
    		write(6,*) "Invalid input: please enter an integer."
    		stop
	end if

! Check to see if the number is positive, terminate the program if not 
	if (n < 1) then
		write(6,*) "The number is not positive"
		stop
	endif


	write(6,*) "The fibonacci sequence from n = 0 to n = ",n,"is : "
	write(6, '(i0,a,i0,a)', advance = 'NO') f0, ", ", f1, ", "
		
	do i=2,n
		call fibonacci(f1,f0)
	
		write(6, "(i0,a)", advance = 'NO') f1, ", "

! Make a new line of text after every 10 numbers  

		if (mod((i+1),10) == 0) write(6,*)
	end do


end program fibonacci_prog
