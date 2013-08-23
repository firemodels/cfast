MODULE PRECISION_PARAMETERS
 
! Set important parameters having to do with variable precision and array allocations
 
IMPLICIT NONE
 
CHARACTER(255), PARAMETER :: precid='$Id$'
CHARACTER(255), PARAMETER :: precrev='$Revision$'
CHARACTER(255), PARAMETER :: precdate='$Date$'

! Precision of "Four Byte" and "Eight Byte" reals

INTEGER, PARAMETER :: FB = SELECTED_REAL_KIND(6)
INTEGER, PARAMETER :: EB = SELECTED_REAL_KIND(12)

! Often used numbers

REAL(EB) :: PI

CONTAINS

SUBROUTINE SET_OFTEN_USED

PI=4._EB*ATAN(1.0_EB)

END SUBROUTINE SET_OFTEN_USED

END MODULE PRECISION_PARAMETERS
