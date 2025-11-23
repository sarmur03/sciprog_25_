! Find the greatest common divisor between two inputs a and b given by the user. The inputs must be positive integers
! Two functions are created; one is iterative and the other is recursive
! The output of each is printed to the screen for comparison
module gcdfunctions
	implicit none
	contains ! Use contains to write full function definition in module; use interface to define function at end
		function iterativeGCD(a,b) result(answer)
			implicit none
			integer(kind=4),intent(in) :: a,b
			integer(kind=4) :: temp, tempa, tempb, answer
! Need two temp variables because values of intent(in) variables cant be modified
				tempa = a
				tempb = b
				do while(tempb .ne. 0)
					temp = tempb
					tempb = mod(tempa, tempb)
					tempa = temp
				end do
				answer = tempa
				return
		end function iterativeGCD

		recursive function recursiveGCD(a,b) result(answer)
			integer(kind=4),intent(in) :: a,b
			integer(kind=4) :: answer
			
				if (b .eq. 0) then
					answer = a
				else 
					answer = recursiveGCD(b, mod(a,b))
				end if

				return

		end function recursiveGCD
	
end module gcdfunctions



program gcd
	use gcdfunctions
	implicit none
	integer (kind = 4) :: a,b,error
	
	error = 1
	write(6,*) "Please input two positive integers"

	do while (error .ne. 0)
		read(5,*,iostat = error) a,b

! Checking values given are of type integer

		if (error .ne. 0) then
			write(6,*) "Non-integer values entered. Please try again"
		end if
	end do

! Checking for non-positive entries

	if(a .le. 0 .or. b .le. 0) then
		write(6,*)"Non-positive values entered"
		stop
	end if 

! Call functions

	write(6,'(a,i3,a,i3,a,i3)') "Iterative GCD( ", a, "," , b , ") = ", iterativeGCD(a,b)
	write(6,'(a,i3,a,i3,a,i3)') "Recursive GCD( ", a, "," , b , ") = ", recursiveGCD(a,b)


end program gcd
