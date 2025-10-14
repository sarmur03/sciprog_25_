
program sum
! Declare variables
   integer (kind=4) :: i
   real (kind=4) :: sum1, sum2, diff
   

! First sum: We add 1 + 1/2 +1/3 + ... +1/1000 and store the result in sum1
   sum1 = 0.0
   do i=1,1000
      sum1 = sum1  + 1.0/real(i)	
   end do


! Second sum: We add 1/1000 + 1/999 + 1/998 + ... +1/2 + 1 and store the result in sum2
   sum2 = 0.0
   do i=1000,1,-1
      sum2 = sum2 + 1.0/real(i)
   end do

! Output the result of each sum 

   write(6,*) ' Sum1=',sum1
   write(6,*) ' Sum2=',sum2

! Find the difference between both sums 
   diff = sum1 - sum2

   write(6,*) ' Difference between the two is ',diff

end program sum
