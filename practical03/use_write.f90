program printing 

	implicit none
	integer (kind = 4) :: i1 = 10, i2 = 200
	real (kind = 4) :: fl1 = 1.11, fl2 = 2.2222

! Two different ways of writing this statement. One such way is this

	write(6,'(a, 2i6, /, a, f8.2, f10.3 )') 'Two ints', i1, i2, 'and two floats', fl1, fl2
! Another way I can write it is this. To see the output, uncomment out the following line before compiling
! and executing:

	write(6,*) 'Two ints ', i1, i2, ' and two floats ', fl1, fl2
end program printing
