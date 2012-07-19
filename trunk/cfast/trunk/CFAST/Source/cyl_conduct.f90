    subroutine getcyltemp(x,wtemp,nx,rad,tempx)
    implicit none
    real*8, intent(in) :: x, rad
    integer, intent(in) :: nx
    real*8, intent(in), dimension(nx) :: wtemp
    real*8, intent(out) :: tempx

    real*8 :: dr, r, rint, factor
    integer :: left, right

    dr = rad/nx
    r = rad-x
    if(r<=dr/2.0)then
        tempx = wtemp(1)
        return
    endif
    if(r>=rad-dr/2.0)then
        tempx = wtemp(nx)
        return
    endif
    rint = r/dr-0.5
    left = int(rint)+1
    left=max(min(left,nx),1)
    right = left + 1
    right=max(min(right,nx),1)
    factor = (rint-int(rint))
    tempx = factor*wtemp(right) + (1.0-factor)*wtemp(left)

    end subroutine getcyltemp

    subroutine cylcnduct(wtemp,nx,wfluxin,dt,wk,wrho,wspec,diam)

    !     arguments: wtemp    wall temperature profile
    !                nx       number of nodes
    !                wfluxin  flux striking interior wall
    !                dt       time step interval from last valid solution point
    !                wrho     wall density
    !                diam     diameter of cable

    implicit none

    integer, intent(in) :: nx
    real*8, intent(in)  :: dt,wrho, wk, wspec, diam
    real*8, intent(in)  :: wfluxin
    real*8, intent(inout), dimension(nx) :: wtemp

    ! declare local variables

    integer :: nn, i, ii, nr, niter, iter
    parameter (nn = 50)
    real*8, dimension(nn) :: aim1, ai, aip1, tnew
    real*8, dimension(nn) :: cc, dd
    real*8 :: alpha, dr, factor, dt_iter


    nr = nn
    dr = (diam/2.0d0)/nr
    alpha = wk / (wspec*wrho)
    dt_iter = min(dt,0.1)
    niter = dt/dt_iter + 0.5
    dt_iter=dt/niter
    factor = 2.0*alpha*dt_iter/dr**2

    do iter=1,niter     
        do i = 1, nr
            cc(i)=factor*(i-1.0d0)/(2.0d0*i-1.0d0)
            dd(i)=factor*(i-0.0d0)/(2.0d0*i-1.0d0)
        end do

        do i = 1, nr-1
            aim1(i) = -cc(i)
            ai(i) = 1.0d0 + factor
            aip1(i) = -dd(i)
            tnew(i) = wtemp(i)
        end do

        aim1(nr) = -cc(nr)
        ai(nr) = 1.0d0 + cc(nr)
        aip1(nr) = -dd(nr)
        tnew(nr) = wtemp(nr) + dd(nr)*wfluxin*dr/wk

        ! aim1(nr) = -1.0
        ! ai(nr) = 1.0d0
        ! aip1(nr) = 0.0
        ! tnew(nr) = wfluxin*dr/wk
        ! now perform an l-u factorization of this matrix (see atkinson p.455)
        ! note: matrix is diagonally dominant so we don't have to pivot

        ! note we do the following in case a(1) is not 1

        aip1(1) = aip1(1) / ai(1)
        do i = 2, nx - 1
            ai(i) = ai(i) - aim1(i) * aip1(i-1)
            aip1(i) = aip1(i) / ai(i)
        end do
        ai(nx) = ai(nx) - aim1(nx) * aip1(nx-1)

        ! now construct guess at new temperature profile

        ! forward substition
        tnew(1) = tnew(1) / ai(1)
        do i = 2, nx
            tnew(i) = (tnew(i)-aim1(i)*tnew(i-1)) / ai(i)
        end do

        ! backward substition
        do i = nx - 1, 1, -1
            tnew(i) = tnew(i) - aip1(i) * tnew(i+1)
        end do

        do i = 1, nx
            wtemp(i) = tnew(i)
        end do
    end do
    return
    end
    subroutine get_flux(t,temp_cable,temp_amb,temp_shroud,flux_out)

    real*8, intent(in) :: t,temp_cable,temp_amb
    real*8, intent(out) :: flux_out,temp_shroud

    real*8 :: factor, factor2, sigma, temp_gas

    sigma = 5.67/10.0**8

    if(t>=0.0.and.t<=70.0)then
        factor = (t-0.0)/70.0
        factor2 = ((t-0.0)*210.0 + (70.0-t)*24.0)/70.0
        ! else if(t>70.0.and.t<=820.0)then
    else if(t>70.0)then
        factor = 1.0
        factor2 = 210.0
        ! else if(t>820.0.and.t<=1240.0)then
        !   factor = ((t-820.0)*0.62 + (1240.0-t)*1.0)/(1240.0-820.0)
        !   factor2 = ((t-820.0)*150.0 + (1240.0-t)*210.0)/(1240.0-820.0)
        ! else if(t>1240.0)then
        !   factor = 0.62
        !   factor2 = 150.0
    else
        factor = 0.0
        factor2 = 24.0
    endif

    temp_shroud = 273.0 + 24.0*(1.0-factor)+480.0*factor
    temp_gas = factor2 + 273.0
    flux_out = .95*sigma*(temp_shroud**4-temp_cable**4)
    ! flux_out = flux_out + 10*(temp_shroud - temp_cable)

    end subroutine get_flux      

