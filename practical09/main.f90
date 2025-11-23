! This program reads a matrix from an input file and checks whether the matrix is magic (i.e. rows,columns and diagonals sum to
! the same value M)
!
! The function is defined in a seperate headerfile called magic_square.fh

include 'magic_square.fh'

program magsq
    use msquare
    implicit none
    integer (kind=4) n,i,ierr,num(100)
    integer (kind=4), allocatable :: magicSquare(:,:)
    character (len=6) :: text
    character (len=100) :: filename, line


! Opening the file
	write(6,*)"Enter a filename"
	read(5,*)filename
	
	ierr=0
	open(unit=1,file = filename, form = "Formatted", access = "Sequential", action = "Read",status = "Old", iostat = ierr)

	if(ierr .ne. 0) then
		write(6,*)"Can't open file"
		stop
	end if 

! Calculate number of lines in input file

	n = 0
	do 
		read(1,*,iostat=ierr)
		if (ierr .ne. 0) exit 
		n = n+1
	end do
	rewind(unit=1) ! Go back to initial point
	write(6,*)"There is ", n, " lines in input file"


! TODO: Allocating a matrix for storing the magic square
! as an array of pointers, where each pointer is a row
	allocate(magicSquare(n,n))

!
! TODO: Read in the rows from each line
!

	do i=1,n,1
		read(1,*,iostat=ierr) magicSquare(i,:)
	end do

	if(isMagicSquare(magicSquare,n)) then
		text = "IS"
	else
		text = "IS NOT"	
	end if

	write(6,*) "This square ", trim(text), " a magic square."


! TODO: Freeing each row separately before freeing the array of pointers
	deallocate(magicSquare)
	close(unit = 1, status = "KEEP")

	
end program magsq

