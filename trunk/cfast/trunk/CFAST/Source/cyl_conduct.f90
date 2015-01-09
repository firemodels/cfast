
! --------------------------- get_cylinder_temperature -------------------------------------------

    subroutine get_cylinder_temperature(x,wtemp,nx,rad,tempx)
    use precision_parameters
    implicit none
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
    endif
    if(r>=rad-dr/2.0_eb)then
        tempx = wtemp(nx)
        return
    endif
    rint = r/dr-0.5_eb
    left = int(rint)+1
    left=max(min(left,nx),1)
    right = left + 1
    right=max(min(right,nx),1)
    factor = (rint-int(rint))
    tempx = factor*wtemp(right) + (1.0_eb-factor)*wtemp(left)

    end subroutine get_cylinder_temperature

! --------------------------- cylindrical_conductive_flux -------------------------------------------

    subroutine cylindrical_conductive_flux (wtemp,nx,wfluxin,dt,wk,wrho,wspec,diam)

    !     arguments: wtemp    wall temperature profile
    !                nx       number of nodes
    !                wfluxin  flux striking interior wall
    !                dt       time step interval from last valid solution point
    !                wrho     wall density
    !                diam     diameter of cable

    use precision_parameters
    implicit none

    integer, intent(in) :: nx
    real(eb), intent(in)  :: dt, wrho, wk, wspec, diam
    real(eb), intent(in)  :: wfluxin
    real(eb), intent(inout), dimension(nx) :: wtemp

    ! declare local variables

    integer :: nn, i, nr, niter, iter
    parameter (nn = 50)
    real(eb), dimension(nn) :: aim1, ai, aip1, tnew
    real(eb), dimension(nn) :: cc, dd
    real(eb) :: alpha, dr, factor, dt_iter

    nr = nn
    dr = (diam/2.0_eb)/nr
    alpha = wk/(wspec*wrho)
    dt_iter = min(dt,0.1_eb)
    niter = dt/dt_iter + 0.5_eb
    dt_iter=dt/niter
    factor = 2.0_eb*alpha*dt_iter/dr**2

    do iter=1,niter     
       do i = 1, nr
          cc(i)=factor*real(i-1,eb)/(2.0_eb*real(i,eb)-1.0_eb)
          dd(i)=factor*real(i,eb)/(2.0_eb*real(i,eb)-1.0_eb)
       end do

      do i = 1, nr-1
          aim1(i) = -cc(i)
          ai(i) = 1.0_eb + factor
          aip1(i) = -dd(i)
          tnew(i) = wtemp(i)
      end do

       aim1(nr) = -cc(nr)
       ai(nr) = 1.0_eb + cc(nr)
       aip1(nr) = -dd(nr)
       tnew(nr) = wtemp(nr) + dd(nr)*wfluxin*dr/wk

       ! aim1(nr) = -1.0
       ! ai(nr) = 1.0_eb
       ! aip1(nr) = 0.0
       ! tnew(nr) = wfluxin*dr/wk
       ! now perform an l-u factorization of this matrix (see atkinson p.455)
       ! note: matrix is diagonally dominant so we don't have to pivot

       ! note we do the following in case a(1) is not 1

       aip1(1) = aip1(1)/ai(1)
       do i = 2, nx - 1
          ai(i) = ai(i) - aim1(i)*aip1(i-1)
          aip1(i) = aip1(i)/ai(i)
       end do
       ai(nx) = ai(nx) - aim1(nx)*aip1(nx-1)

        ! now construct guess at new temperature profile

        ! forward substition
       tnew(1) = tnew(1)/ai(1)
       do i = 2, nx
          tnew(i) = (tnew(i)-aim1(i)*tnew(i-1))/ai(i)
       end do

       ! backward substition
       do i = nx - 1, 1, -1
          tnew(i) = tnew(i) - aip1(i)*tnew(i+1)
       end do

       do i = 1, nx
          wtemp(i) = tnew(i)
       end do
    end do
    return
    end

! --------------------------- get_flux -------------------------------------------

    subroutine get_flux(t,temp_cable,temp_shroud,flux_out)
    
    use precision_parameters
    implicit none

    real(eb), intent(in) :: t,temp_cable
    real(eb), intent(out) :: flux_out,temp_shroud

    real(eb) :: factor, factor2, temp_gas

    if(t>=0.0_eb.and.t<=70.0_eb)then
        factor = (t-0.0_eb)/70.0_eb
        factor2 = ((t-0.0_eb)*210.0_eb + (70.0_eb-t)*24.0_eb)/70.0_eb
        ! else if(t>70.0.and.t<=820.0)then
    else if(t>70.0_eb)then
        factor = 1.0_eb
        factor2 = 210.0_eb
        ! else if(t>820.0.and.t<=1240.0)then
        !   factor = ((t-820.0)*0.62 + (1240.0-t)*1.0)/(1240.0-820.0)
        !   factor2 = ((t-820.0)*150.0 + (1240.0-t)*210.0)/(1240.0-820.0)
        ! else if(t>1240.0)then
        !   factor = 0.62
        !   factor2 = 150.0
    else
        factor = 0.0_eb
        factor2 = 24.0_eb
    endif

    temp_shroud = kelvin_c_offset + 24.0_eb*(1.0_eb-factor)+480.0_eb*factor
    temp_gas = factor2 + kelvin_c_offset
    flux_out = 0.95_eb*sigma*(temp_shroud**4-temp_cable**4)
    ! flux_out = flux_out + 10*(temp_shroud - temp_cable)

    end subroutine get_flux      

