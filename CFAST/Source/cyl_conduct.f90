module cylinder_routines

    use precision_parameters

    implicit none

    private

    public cylindrical_conductive_flux, get_cylinder_temperature

    contains

! --------------------------- cylindrical_conductive_flux -------------------------------------------

    subroutine cylindrical_conductive_flux (iwbound,tempin,wtemp,nr,wfluxin,dt,wk,wrho,wspec,diam,tgrad)

    !     arguments: wtemp    cable temperature profile
    !                nr       number of nodes
    !                wfluxin  flux striking cable
    !                dt       time step interval from last valid solution point
    !                wrho     cable density
    !                diam     cable diameter

    integer, intent(in) :: nr, iwbound
    real(eb), intent(in)  :: dt, wrho, wk, wspec, diam, tempin
    real(eb), intent(in)  :: wfluxin
    real(eb), intent(inout), dimension(nr) :: wtemp
    real(eb), intent(out), dimension(2) :: tgrad

    ! declare local variables

    integer :: i, niter, iter
    real(eb), dimension(nr) :: aim1, ai, aip1, tnew
    real(eb), dimension(nr) :: cc, dd
    real(eb) :: alpha, dr, factor, dt_iter
    real(eb) :: ddif(2)

    dr = (diam/2.0_eb)/nr
    alpha = wk/(wspec*wrho)
    dt_iter = min(dt,0.1_eb)
    if(dt_iter.gt.0.0_eb)then
       niter = dt/dt_iter + 0.5_eb
       dt_iter=dt/niter
       factor = 2.0_eb*alpha*dt_iter/dr**2
    else
       niter = 0
    end if

    do iter=1,niter
       do i = 1, nr
          cc(i)=factor*real(i-1,eb)/(2.0_eb*real(i,eb)-1.0_eb)
          dd(i)=factor*real(i,eb)/(2.0_eb*real(i,eb)-1.0_eb)
       end do
! if iwbound==3 then constant temperature boundary condition
!                    flux boundary condition otherwise

       aim1(1:nr-1) = -cc(1:nr-1)
       ai(1:nr-1) = 1.0_eb + factor
       aip1(1:nr-1) = -dd(1:nr-1)
       tnew(1:nr-1) = wtemp(1:nr-1)

       aim1(nr) = -cc(nr)
       ai(nr) = 1.0_eb + cc(nr)
       aip1(nr) = -dd(nr)

       if(iwbound==3)then
          cc(nr) = 0.0_eb
          dd(nr) = 0.0_eb
          tnew(nr) = tempin
       else
          tnew(nr) = wtemp(nr) + dd(nr)*wfluxin*dr/wk
       end if

       ! aim1(nr) = -1.0
       ! ai(nr) = 1.0_eb
       ! aip1(nr) = 0.0
       ! tnew(nr) = wfluxin*room_depth/wk
       ! now perform an LU factorization of this matrix (see atkinson p.455)
       ! note: matrix is diagonally dominant so pivoting is not necessary

       ! note we do the following in case a(1) is not 1

       aip1(1) = aip1(1)/ai(1)
       do i = 2, nr - 1
          ai(i) = ai(i) - aim1(i)*aip1(i-1)
          aip1(i) = aip1(i)/ai(i)
       end do
       ai(nr) = ai(nr) - aim1(nr)*aip1(nr-1)

        ! now construct guess at new temperature profile

        ! forward substition
       tnew(1) = tnew(1)/ai(1)
       do i = 2, nr
          tnew(i) = (tnew(i)-aim1(i)*tnew(i-1))/ai(i)
       end do

       ! backward substition
       do i = nr - 1, 1, -1
          tnew(i) = tnew(i) - aip1(i)*tnew(i+1)
       end do

       wtemp(1:nr) = tnew(1:nr)
    end do
    ! estimate temperature gradient at target surface by constructing a quadratic polynomial that
    ! interpolates first three data points in the temperature profile using divided differences.

    ! first divided difference
    ddif(1) = (wtemp(nr-1)-wtemp(nr))/dr
    ddif(2) = (wtemp(nr-2)-wtemp(nr-1))/dr

    ! second divided difference
    ddif(2) = (ddif(2)-ddif(1))/(2.0_eb*dr)

    tgrad(1) = (ddif(1)-ddif(2)*dr)
    tgrad(2) = (wtemp(nr-1)-wtemp(nr))/dr
    return

    end subroutine cylindrical_conductive_flux

! --------------------------- get_cylinder_temperature -------------------------------------------

    subroutine get_cylinder_temperature(x,wtemp,nx,rad,tempx)

    real(eb), intent(in) :: x, rad
    integer, intent(in) :: nx
    real(eb), intent(in), dimension(nx) :: wtemp
    real(eb), intent(out) :: tempx

    real(eb) :: dr, r, rint, factor
    integer :: left, right

    dr = rad/nx
    r = rad-x
    if(r<=dr/2.0_eb)then
        tempx = wtemp(1)
        return
    end if
    if(r>=rad-dr/2.0_eb)then
        tempx = wtemp(nx)
        return
    end if
    rint = r/dr-0.5_eb
    left = int(rint)+1
    left=max(min(left,nx),1)
    right = left + 1
    right=max(min(right,nx),1)
    factor = (rint-int(rint))
    tempx = factor*wtemp(right) + (1.0_eb-factor)*wtemp(left)

    end subroutine get_cylinder_temperature

end module cylinder_routines