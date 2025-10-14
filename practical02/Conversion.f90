program conversion 
    implicit none

! Declare variable types and names due to the fact that we have written implicit none above (means we have to
! explicitly declare all variables before use)

    integer (kind=4) :: i,inum,tmp,numdigits
    character (len=32) :: binnum
    real(kind=4) :: fnum
    character (len=32) :: binnum_reverse
    integer (kind=4) :: remainder, length, j

    ! Intialise 4-byte integer called inum
    inum = 33554431
    ! Convert 4-byte integer inum to 4-byte float fnum
    fnum = real(inum)       

    ! Binary converter using modulus   (given loop)
    i=1
    tmp=inum
    do while (tmp > 0)
        if(mod(tmp,2)==0) then
            binnum(i:i) = '0'
        else
            binnum(i:i) = '1'
        end if
        tmp = tmp/2
        i = i+1
    end do

    binnum=trim(binnum) !Removes trailing blank characters of a string.

    !Reverse the string (given loop)
    length = len(binnum)
    do i = 1,length
        j = length - i + 1
        binnum_reverse(i:i) = binnum(j:j)
    end do


    ! TODO: Loop created to check the number of binary digits
    ! Since we have log_k (x) = log_a (x) / log_a (b)  , where _ represents a subscript letter, or in this case, 
    ! the base of the log, we want numdigits, which is equal to log_2 (fnum). Hence we have the formula below.
    numdigits = nint(log(fnum)/log(2.0)) 
    write(6,*) ' The number of digits is ',numdigits

    write(6,*)  'inum=', inum ,', fnum=', fnum
    write(6,*)  'binary=', binnum_reverse

end program conversion
