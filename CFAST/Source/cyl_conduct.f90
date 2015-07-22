
! --------------------------- get_cylinder_temperature -------------------------------------------

    subroutine get_cylinder_temperature(x,wtemp,nx,rad,tempx)
    use precision_parameters
    implicit none
    real(eb), intent(in) :: x, rad
    integer, intent(in) :: nx
    real(eb), intent(in), dimension(nx) :: wtemp
    real(eb), intent(out) :: tempx

    real(eb) :: room_depth, r, rint, factor
    integer :: left, right

    room_depth = rad/nx
    r = rad-x
    if(r<=room_depth/2.0_eb)then
        tempx = wtemp(1)
        return
    endif
    if(r>=rad-room_depth/2.0_eb)then
        tempx = wtemp(nx)
        return
    endif
    rint = r/room_depth-0.5_eb
    left = int(rint)+1
    left=max(min(left,nx),1)
    right = left + 1
    right=max(min(right,nx),1)
    factor = (rint-int(rint))
    tempx = factor*wtemp(right) + (1.0_eb-factor)*wtemp(left)

    end subroutine get_cylinder_temperature

! --------------------------- cylindrical_conductive_flux -------------------------------------------

    subroutine cylindrical_conductive_flux (wtemp,nr,wfluxin,dt,wk,wrho,wspec,diam)

    !     arguments: wtemp    wall temperature profile
    !                nr       number of nodes
    !                wfluxin  flux striking interior wall
    !                dt       time step interval from last valid solution point
    !                wrho     wall density
    !                diam     diameter of cable

    use precision_parameters
    implicit none

    integer, intent(in) :: nr
    real(eb), intent(in)  :: dt, wrho, wk, wspec, diam
    real(eb), intent(in)  :: wfluxin
    real(eb), intent(inout), dimension(nr) :: wtemp

    ! declare local variables

    integer :: i, niter, iter
    real(eb), dimension(nr) :: aim1, ai, aip1, tnew
    real(eb), dimension(nr) :: cc, dd
    real(eb) :: alpha, room_depth, factor, dt_iter

    room_depth = (diam/2.0_eb)/nr
    alpha = wk/(wspec*wrho)
    dt_iter = min(dt,0.1_eb)
    niter = dt/dt_iter + 0.5_eb
    dt_iter=dt/niter
    factor = 2.0_eb*alpha*dt_iter/room_depth**2

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
       tnew(nr) = wtemp(nr) + dd(nr)*wfluxin*room_depth/wk

       ! aim1(nr) = -1.0
       ! ai(nr) = 1.0_eb
       ! aip1(nr) = 0.0
       ! tnew(nr) = wfluxin*room_depth/wk
       ! now perform an l-u factorization of this matrix (see atkinson p.455)
       ! note: matrix is diagonally dominant so we don't have to pivot

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

       do i = 1, nr
          wtemp(i) = tnew(i)
       end do
    end do
    return
    end
