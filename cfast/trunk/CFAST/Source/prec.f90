module precision_parameters
 
! set important parameters having to do with variable precision and array allocations
 
implicit none
 
character(255), parameter :: precid='$id: prec.f90 968 2013-08-23 13:50:35Z gforney $'
character(255), parameter :: precrev='$revision: 968 $'
character(255), parameter :: precdate='$date: 2013-08-23 09:50:35 -0400 (fri, 23 aug 2013) $'

! precision of "four byte" and "eight byte" reals

integer, parameter :: fb = selected_real_kind(6)
integer, parameter :: eb = selected_real_kind(12)

! often used numbers

real(eb) :: pi, sigma, third, fourpi, pio2

contains

subroutine set_often_used

pi=4._eb*atan(1.0_eb)
sigma = 5.67e-8_eb
third = 1.0_eb/3.0_eb
fourpi = 4.0_eb*pi
pio2 = pi/2.0_eb

end subroutine set_often_used

end module precision_parameters
