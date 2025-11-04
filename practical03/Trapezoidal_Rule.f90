program trapezoidal

! This is a program used to approximate the area under the function f(x) = tan(x) from 0 < x < pi/3
! The domain is split into twelve subintervals (N) and f(x) is evaluated at each point f(x0),f(x1)...f(x12)
  

! Variable Declaration: All variables must be declared as stated by the 'implicit none' line. 
	implicit none
	integer (kind = 4) :: N = 12, i
	real (kind = 4) :: a = 0.0, b_deg = 60.0
	real (kind = 4) :: b_rad, area, mult_rad, pi, error

! Converting b_deg to radians. Note that pi is calculated manually as it is not included as a constant in FORTRAN
	pi = atan(1.0)*4.0
	b_rad = (pi * b_deg)/180.0
	write(6,'(a,f7.4, a)') "60 degrees in radians is equal to", b_rad, "rad"

! Sum tan(a) + tan(b)

	area = tan(a) + tan(b_rad)
	write(6,'(a,f7.4)') "Initial area f(x0) + f(x12) is equal to", area
	

! Calculate 2*( f(x1) + f(x2) + ... + f(x11) )
! We start at i = 5 because 60/12 = 5 	

	do i = 5,55,5
		area = area + 2*tan((pi * i)/180.0)
		write(6,*) "New area of a(" , i/5 , ") = " , area
	end do 

! Multiply the area by pi/3
	mult_rad = (pi* ((b_deg - a)/(2*N)))/180.0
	area = mult_rad*area

! Approximate result
write(6,*) "Trapezoidal result is " , area

! Real result

write(6,*) "Real result is " , log(2.0)

! Not asked for in the assignment but added anyway :)
error = abs(area - log(2.0))
write(6,*) "The error in our approximation is ", error
end program trapezoidal
