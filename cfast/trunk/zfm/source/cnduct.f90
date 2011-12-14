subroutine cnduct(update,tempin,tempout,dt,wall,&
                  wfluxout,iwbound,tgrad)
  use precision
  use zonedata
  implicit none

  integer, intent(in) :: update, iwbound
  real(kind=dd), intent(in) :: tempin,tempout, dt, wfluxout
  type(wall_data), pointer :: wall

!
!     Arguments: UPDATE   We don't keep solution unless UPDATE is 1 OR 2
!                TEMPIN   Temperature at interior wall
!                TEMPOUT  Temperature at exterior wall, not used now
!                DT       Time step interval from last valid solution point
!                WFLUXOUT Flux striking exterior wall
!                IWBOUND  Type of boundary condition for exterior wall
!                         (1=constant temperature, 2=insulated,
!                          3=flux based on ambient temperature on outside wall
!                TGRAD    Temperature gradient
!


!  DECLARE LOCAL VARIABLES

  real(kind=dd), allocatable, dimension(:) :: a, b, c
  real(kind=dd), allocatable, dimension(:) :: tnew
  real(kind=dd) :: tgrad
  real(kind=dd) :: xkrhoc, s, hi, him1
  real(kind=dd), pointer, dimension(:) :: wtemp
  real(kind=dd), pointer, dimension(:) :: walldx
  integer :: nx, i 

  
  nx = wall%n
  walldx => wall%dx
  wtemp => wall%wtemp
  allocate(a(nx),b(nx),c(nx),tnew(nx))
  tnew(2:nx-1) = wtemp(2:nx-1)

!   set up tri-diagonal coefficient matrix

!   setup first row

  a(1) = 1.0_dd
  b(1) = 0.0_dd
  c(1) = 0.0_dd
  tnew(1) = tempin

  xkrhoc = wall%k / (wall%c*wall%rho)
  s = 2.0_dd * dt * xkrhoc
  do i = 2, nx-1
    hi = wall%dx(i)
    him1 = wall%dx(i-1)
    a(i) = 1.0_dd + s / (hi*him1)
    b(i) = -s / (him1*(hi+him1))
    c(i) = -s / (hi*(hi+him1))
  end do

!   setup last row, note: last row depends on form of boundary condition

  if (iwbound.eq.2) then

!   insulated boundary condition

    a(nx) = 1.0_dd
    b(nx) = -1.0_dd
    c(nx) = 0.0_dd
    tnew(nx) = 0.0_dd
   else

!   flux boundary condition (using lagged temperatures

    a(nx) = 1.0_dd
    b(nx) = -1.0_dd
    c(nx) = 0.0_dd
    tnew(nx) = walldx(nx-1) * wfluxout / wall%k
  end if
     
!  NOW PERFORM AN L-U FACTORIZATION OF THIS MATRIX (see atkinson p.455)
!    NOTE: MATRIX IS DIAGONALLY DOMINANT SO WE DON'T HAVE TO PIVOT

!  note we do the following in case a(1) is not 1

   c(1) = c(1) / a(1)
   do i = 2, nx - 1
     a(i) = a(i) - b(i) * c(i-1)
     c(i) = c(i) / a(i)
   end do
   a(nx) = a(nx) - b(nx) * c(nx-1)

!  NOW CONSTRUCT GUESS AT NEW TEMPERATURE PROFILE

!  FORWARD SUBSTITION

  tnew(1) = tnew(1) / a(1)
  do i = 2, nx
    tnew(i) = (tnew(i)-b(i)*tnew(i-1)) / a(i)
  end do

!  BACKWARD SUBSTITION

  do i = nx - 1, 1, -1
    tnew(i) = tnew(i) - c(i) * tnew(i+1)
  end do

!   we ignnore solution if update is 0

  if (update.ne.0)wtemp(1:nx) = tnew(1:nx)

  tgrad = (tnew(2)-tnew(1)) / walldx(1)

  return
end subroutine cnduct
