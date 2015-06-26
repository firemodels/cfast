module conduction_routines
    use precision_parameters
    use wallptrs
    use cenviro
    use cfast_main
    use opt
    use wnodes
    use convection_routines

    use cparams, only: nnodes
    implicit none
    
    private
    
    public conduction, conductive_flux
    
    contains
   
! --------------------------- conduction -------------------------------------------

    subroutine conduction(update,dt,flxtot,delta)

    !     routine: conduction (main conduction routine)
    !     purpose: interface between calculate_residuals and the conduction calculation.
    !              for each active wall surface in each routine this
    !              routine calculates the residual function 
    !               q'' + k dt/dx, which when zero is simply fourier's
    !              law of heat conduction.
    !     arguments: update  we don't keep solution unless update is 1 or 2. if update is 2 then 
    !                we don't calculate delta or use flxtot
    !                dt time step interval from last valid solution point
    !                flxtot  total flux striking walls
    !                delta   the residual of q'' + k dt/dx
    !     revision: $revision: 464 $
    !     revision date: $date: 2012-06-29 15:41:23 -0400 (fri, 29 jun 2012) $


    integer, intent(in) :: update
    real(eb), intent(in) :: dt, flxtot(nr,nwal)
    real(eb), intent(out) :: delta(*)
    
    real(eb) :: tgrad(2), vtgrad0(4*nr), vtgrad(4*nr)
    save vtgrad0

    real(eb) :: twint, twext, tgas, wfluxin, wfluxout, wfluxsave, frac, yb, yt, dflor, yy, fu, fluxu, fluxl, tderv
    integer :: ibeg, iend, iw, iroom, iwall, icond, iweq, iwb, nwroom, jj, j, ieq

    type(room_type), pointer :: roomptr

    integer, dimension(nwal) :: irevwc = (/2,1,3,4/)

    ! solve conduction problem for all walls

    ibeg = 1
    iend = nwalls

    ! if the reduced jacobian option is on and dassl is computing a jacobian
    ! then solve a conduction problem only if dassl is varying a wall temperature

    if(option(fmodjac)==on.and.jaccol>0)then
        if(izeqmap(jaccol,1)==eqwt)then
            ! a wall temperature is being perturbed so solve that walls conduction problem
            ibeg = jaccol - nofwt
            iend = ibeg
        else
            ! some other variable is being perturbed, so don't solve any conduction problems
            ibeg = 1
            iend = 0
        endif
    endif

    do iw = ibeg, iend
        iroom = izwall(iw,w_from_room)
        iwall = izwall(iw,w_from_wall)
        icond = nofwt + iw

        if (adiabatic_wall.or..not.surface_on_switch(iwall,iroom)) then
            vtgrad(iw) = 0.0_eb
        else
            roomptr => roominfo(iroom)

            ! use exterior wall temperature from last time step to ...
            twint = zzwtemp(iroom,iwall,1)
            twext = zzwtemp(iroom,iwall,2)
            tgas = exterior_temperature
            iweq = izwmap2(iwall,iroom) - nofwt
            iwb = izwall(iweq,w_boundary_condition)

            ! compute flux seen by exterior of wall
            if (iwb==3) then

                ! back wall is connected to the outside

                call convective_flux (irevwc(iwall),tgas,twext,wfluxout)
                wfluxout = wfluxout + sigma*(tgas**4-twext**4)
                wfluxsave = wfluxout
                if(izheat(iroom)/=0.and.iwall/=1.and.iwall/=2)then

                    ! back wall is connected to rooms defined by izhtfrac with fractions defined by zzhtfrac.
                    !  if izheat(iroom) is not zero then nwroom better not be zero!  nwroom should always be zero
                    ! for iwall=3 and iwall=4
                    wfluxout = 0.0_eb
                    nwroom = izhtfrac(iroom,0)
                    do jj = 1, nwroom
                        j = izhtfrac(iroom,jj)
                        frac = zzhtfrac(iroom,j)
                        if(iwall==3)then
                            yb = zzhlay(iroom,lower)
                            yt = roomptr%yceil
                        elseif(iwall==4)then
                            yb = 0.0_eb
                            yt = zzhlay(iroom,lower)
                        endif
                        dflor = roominfo(j)%yflor - roomptr%yflor
                        yy = zzhlay(j,lower) + dflor
                        if(j/=nm1+1)then
                            if(yy>yt)then
                                fu = 0.0_eb
                            elseif(yy<yb)then
                                fu = 1.0_eb
                            else
                                if(yb/=yt)then
                                    fu = (yt-yy)/(yt-yb)
                                else
                                    fu = 0.0_eb
                                endif
                            endif
                            fluxu = fu*flxtot(j,3)
                            fluxl = (1.0_eb-fu)*flxtot(j,4)
                        else
                            fluxu = wfluxsave
                            fluxl = 0.0_eb
                        endif
                        wfluxout = wfluxout + frac*(fluxu + fluxl)
                    end do
                endif
            endif
            call conductive_flux (update,twint,twext,dt,fkw(1,iwall,iroom),cw(1,iwall,iroom),rw(1,iwall,iroom), &
                twj(1,iroom,iwall),walldx(1,iroom,iwall),numnode(1,iwall,iroom),nslb(iwall,iroom),wfluxin,wfluxout,iwb,tgrad,tderv)

            ! store wall gradient
            vtgrad(iw) = tgrad(2)

            ! compute partial of wall temperature equation with respect to the wall temperature.  we assume that the
            ! partials of convective heat flux and radiative heat flux with respect to wall temperature have already
            ! been computed.  (if they were not then we wouldn't know heat flux striking the wall!
        end if
    end do

    ! save wall gradients during base call to calculate_residuals
    if(option(fmodjac)==on)then

        ! store wall gradient for later use
        if(jaccol==0)then
            do iw = 1, nwalls
                vtgrad0(iw) = vtgrad(iw)
            end do
        elseif(jaccol>0)then

            ! use saved wall temperature gradient except for conduction problem corresponding to the perturbed wall temperature
            if(izeqmap(jaccol,1)==eqwt)then
                ieq = jaccol - nofwt
            else
                ieq = 0
            endif
            do iw = 1, nwalls
                if(iw/=ieq)vtgrad(iw) = vtgrad0(iw)
            end do
        endif
    endif                 

    ! dassl will try to force delta to be zero, so that fourier's law, q = -k dt/dx, is satisfied at the wall surface 
    if(update/=2)then
        do iw = 1, nwalls
            icond = nofwt + iw
            iroom = izwall(iw,w_from_room)
            iwall = izwall(iw,w_from_wall)
            delta(icond) = flxtot(iroom,iwall) + vtgrad(iw)*fkw(1,iwall,iroom)
        end do
    endif

    return
    end subroutine conduction

! --------------------------- conductive_flux -------------------------------------------

    subroutine conductive_flux (update,tempin,tempout,dt,wk,wspec,wrho,wtemp,walldx,numnode,nslab,wfluxin,wfluxout,iwbound,&
       tgrad,tderv)


    ! routine:  conductive_flux 
    ! purpose: handles cfast conduction
    ! arguments: update   we don't keep solution unless update is 1 or 2
    !            tempin   temperature at interior wall
    !            tempout  temperature at exterior wall, not used now
    !            dt       time step interval from last valid solution point
    !            wk       wall thermal conductivity
    !            wspec    wall specific heat
    !            wrho     wall density
    !            wtemp    wall temperature profile
    !            walldx   wall position points
    !            numnode  number of nodes in each slab
    !            nslab    number of slabs
    !            wfluxin  flux striking interior wall
    !            wfluxout flux striking exterior wall
    !            iwbound  type of boundary condition for exterior wall (1=constant temperature, 2=insulated, 3=flux based 
    !                     on ambient temperature on outside wall, 4=flux on both interior and exterior walls)
    !            tgrad    temperature gradient
    !            tderv    partial of temperature gradient with respect to wall surface temperature.  
    !                     this number is used to calculate wall jacobian elements.

    real(eb), intent(in) :: wk(*), wspec(*), wrho(*), walldx(*)
    real(eb), intent(out) :: wtemp(*), tgrad(2) 
    integer, intent(in) :: update, nslab, iwbound, numnode(*)

    
    integer :: nx, i, ibeg, iend, islab, nintx, ibreak
    real(eb) :: a(nnodes), b(nnodes), c(nnodes), tnew(nnodes), tderiv(nnodes), ddif(3)
    real(eb) :: tempin, tempout, wfluxin, wfluxout, xkrhoc, s, dt, hi, him1, tderv

    nx = numnode(1)

    ! construct right hand side (rhs) of tri-diagonal system for interior nodes.  rhs at boundary and slab break 
    ! points are defined farther down.
    do i = 2, nx - 1
        tnew(i) = wtemp(i)
    end do

    ! set up tri-diagonal coefficient matrix

    ! setup first row
    if(iwbound/=4)then
        a(1) = 1.0_eb
        b(1) = 0.0_eb
        c(1) = 0.0_eb
        tnew(1) = tempin
    else
        a(1) = 1.0_eb
        b(1) = 0.0_eb
        c(1) = -1.0_eb
        tnew(1) = walldx(1)*wfluxin/wk(1)
    endif

    ! do interior points for each slab
    iend = 0
    do islab = 1, nslab
        nintx = numnode(1+islab)
        xkrhoc = wk(islab)/(wspec(islab)*wrho(islab))
        s = 2.0_eb*dt*xkrhoc
        ibeg = iend + 2
        iend = ibeg + nintx - 1
        do i = ibeg, iend
            hi = walldx(i)
            him1 = walldx(i-1)
            a(i) = 1.0_eb + s/(hi*him1)
            b(i) = -s/(him1*(hi+him1))
            c(i) = -s/(hi*(hi+him1))
        end do
    end do

    ! do break points between each slab
    ibreak = 1
    do islab = 2, nslab
        nintx = numnode(islab)
        ibreak = ibreak + nintx + 1
        b(ibreak) = wspec(islab-1)*wrho(islab-1)/walldx(ibreak-1)
        c(ibreak) = wspec(islab)*wrho(islab)/walldx(ibreak)
        a(ibreak) = -(b(ibreak)+c(ibreak))
        tnew(ibreak) = 0.0_eb
    end do

    ! setup last row, note: last row depends on form of boundary condition
    if (iwbound==1) then

        ! constant temperature boundary condition (if we ever solve for both interior and exterior wall temperatures 
        ! then use change tnew(nx) = tamb to tnew(nx) = tempout)
        a(nx) = 1.0_eb
        b(nx) = 0.0_eb
        c(nx) = 0.0_eb
        tnew(nx) = tempout
    else if (iwbound==2) then

        ! insulated boundary condition
        a(nx) = 1.0_eb
        b(nx) = -1.0_eb
        c(nx) = 0.0_eb
        tnew(nx) = 0.0_eb
    else if (iwbound==3.or.iwbound==4) then

        ! flux boundary condition (using lagged temperatures
        a(nx) = 1.0_eb
        b(nx) = -1.0_eb
        c(nx) = 0.0_eb
        tnew(nx) = walldx(nx-1)*wfluxout/wk(nslab)
    endif

    ! now perform an l-u factorization of this matrix (see atkinson p.455) note: matrix is 
    ! diagonally dominant so we don't have to pivot

    ! note we do the following in case a(1) is not 1
    c(1) = c(1)/a(1)
    do i = 2, nx - 1
        a(i) = a(i) - b(i)*c(i-1)
        c(i) = c(i)/a(i)
    end do
    a(nx) = a(nx) - b(nx)*c(nx-1)
    do i = 1, nx
        tderiv(i) = 0.0_eb
    end do
    tderiv(1) = 1.0_eb

    ! now construct guess at new temperature profile

    ! forward substition
    tnew(1) = tnew(1)/a(1)
    tderiv(1) = tderiv(1)/a(1)
    do i = 2, nx
        tnew(i) = (tnew(i)-b(i)*tnew(i-1))/a(i)
        tderiv(i) = (tderiv(i)-b(i)*tderiv(i-1))/a(i)
    end do

    ! backward substition
    do i = nx - 1, 1, -1
        tnew(i) = tnew(i) - c(i)*tnew(i+1)
        tderiv(i) = tderiv(i) - c(i)*tderiv(i+1)
    end do

    ! we don't keep solution unless update is 1 or 2
    if (update/=0) then
        do i = 1, nx
            wtemp(i) = tnew(i)
        end do

    endif

    ! estimate temperature gradient at wall surface by constructing a quadratic polynomial that
    ! interpolates first three data points in the temperature profile.  we will use divided differences.

    ! first divided difference
    ddif(1) = (tnew(2)-tnew(1))/walldx(1)
    ddif(2) = (tnew(3)-tnew(2))/walldx(2)

    ! second divided difference
    ddif(2) = (ddif(2)-ddif(1))/(walldx(1)+walldx(2))

    tgrad(1) = (ddif(1)-ddif(2)*walldx(1))
    tgrad(2) = (tnew(2)-tnew(1))/walldx(1)
    tderv = tderiv(2)
    return
    end subroutine conductive_flux
    
end module conduction_routines
