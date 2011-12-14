program main
  use precision
! subroutine interp(tsec,t,q,n,qt)

  real(kind=real_kind) :: tsec, qt
  real(kind=real_kind), dimension(100) :: t, q
  integer :: n

  n = 100

  do i = 1, n
    t(i) = i
    q(i) = i**2
  end do
  tsec = 47.2

  call interp(tsec,t,q,n,qt)

end program main