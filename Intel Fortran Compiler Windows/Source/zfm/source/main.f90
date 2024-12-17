program main
  implicit none
  integer :: error
 ! call zonefire("test1.dat",error)
!  call zonefire("test1.dat",error)
!   call zonefire("test1.dat",error)
!   call zonefire("init3.dat",error)
   call zonefire("basezfmtest.dat",error)
!   call zonefire("init2.dat",error)
!  call result
  write(6,*)"finished"
  stop
end program main

