subroutine svplothdr (version, nrooms, nfires)

!
! This routine prints out a header for the smokeview zone fire data file
!
! This routine is called once
!
!  version  - Presently smokeview only supports version=1 .  In the future
!            if the file format changes then change version to allow
!            smokeview to determine how the data file is organized
!  nrooms  - number of rooms in simulation
!  nfires  - number of fires in simulation
!              
  implicit none
  integer, intent(in) :: version, nrooms, nfires

  write(14) version
  write(14) nrooms
  write(14) nfires
  return

end subroutine svplothdr
