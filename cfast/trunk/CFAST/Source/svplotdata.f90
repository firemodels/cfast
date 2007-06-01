subroutine  svplotdata(time,nrooms,pr,ylay,tl,tu,nfires,qdot,height)

!
! This routine records data for the current time step into the smokeview zone fire data file
!
! plotfile - name of file containing zone fire modeling plot data to 
!            be visualized by smokeview
!     time - current time
!   nrooms   number of rooms
!       pr - real array of size nrooms of room pressures
!     ylay - real array of size nrooms of layer interface heights
!       tl - real array of size nrooms of lower layer temperatures 
!       tu - real array of size nrooms of upper layer temperatures 
!   nfires - number of fires
!     qdot - real array of size nfires of fire heat release rates
!   height - real array of size nfires of fire heights
!
  implicit none

  DOUBLE PRECISION, intent(in) :: time
  integer, intent(in) :: nrooms
  DOUBLE PRECISION, intent(in), dimension(nrooms) :: pr, ylay, tl, tu
  integer, intent(in) :: nfires
  DOUBLE PRECISION, intent(in), dimension(nfires) :: qdot, height
  REAL XXTIME, XXPR, XXYLAY, XXTL, XXTU, XXHEIGHT, XXQDOT

  integer :: i

  xxtime = time
  write(14) xxtime

  do i = 1, nrooms
	XXPR = pr(i)
	XXYLAY = ylay(i)
	XXTL = tl(i)
	XXTU = tu(i)
    write(14) XXPR, XXYLAY, XXTL, XXTU
  end do

  do i = 1, nfires
	XXHEIGHT = height(i)
	XXQDOT = qdot(i)
    write(14) XXHEIGHT, XXQDOT
  end do

End subroutine svplotdata
