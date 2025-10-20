! This is a program to approximate tan from 0 to 60 evaluated at 5 degree intervals. It uses a subroutine for the conversion
! of degrees to radians, and stores the value in an array.

!Module with constants. Reminder pi needs to be calculated
module consts
        integer(kind = 4), parameter :: N = 12
        real (kind = 4) :: pi
end module consts

!Module with subroutines
module subroutines
        interface
                subroutine degtorad(deg,rad)
                use consts
                real (kind = 4), intent(in) :: deg
                real (kind = 4), intent(out) :: rad
                end subroutine degtorad
                subroutine traprule(TanArr, area)
                use consts
                real (kind = 4), intent(in) :: TanArr(N+1)
                real (kind = 4), intent(out) :: area
                end subroutine traprule
        end interface
end module subroutines

program Trapezoidal_subroutine
        use consts
        use subroutines
        implicit none
        integer (kind = 4) :: i
        real (kind = 4) :: TanArr(N+1), deg, rad, area

!Calculate the value of pi
pi = atan(1.0) * 4.0

!Need to calculate f(x_i) from i = 0,12. We will do
do i = 1,N+1,1
        deg = (i-1) * 5.0
        call degtorad(deg,rad)
        TanArr(i) = tan(rad)
        write(6,*) "TanArr(", i, ") = ", TanArr(i)
end do


! Now using the trapezoidal rule to calculate the area under the curve
        call traprule(TanArr, area)

        write(6,*) "Trapezoidal result is ", area
        write(6,*) "Real result is ", log(2.0)
end program Trapezoidal_subroutine


! Subroutines defined after end program

subroutine degtorad(deg,rad)
        use consts
	implicit none 
        real (kind = 4), intent(in) :: deg
        real (kind = 4), intent(out) :: rad
        rad = (pi*deg)/180.0
end subroutine degtorad


! Here we use area as a local variable. IT IS DIFFERENT TO AREA IN THE MAIN BODY

subroutine traprule(TanArr, area)
        use consts
	implicit none
        real (kind = 4), intent(in) :: TanArr(N+1)
        real (kind = 4), intent(out) :: area
        integer (kind = 4) :: i
        real (kind = 4) :: mult_rad
        area = TanArr(1) + TanArr(N+1)
        do i = 2,N,1
                area = area + 2.0*TanArr(i)
        end do
        call degtorad(((60.0-0.0)/(2*N)),mult_rad)
        area = mult_rad*area

end subroutine traprule
