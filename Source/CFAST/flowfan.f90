module mflow_routines

    use precision_parameters

    use opening_fractions, only : qcffraction, qcifraction
    use utility_routines, only: d1mach

    use precision_parameters
    use ramp_data
    use cenviro
    use cparams
    use option_data
    use room_data
    use vent_data

    implicit none

    private

    public mechanical_flow, getmventinfo

    contains

! --------------------------- mechanical_flow -------------------------------------------

    subroutine mechanical_flow (tsec, hvpsolv, hvtsolv, tprime, flwmv, deltpmv, delttmv, prprime, nprod, hvacflg, filtered)

    !     routine: mechanical_flow
    !     purpose: physical interface routine to calculate flow through all forced vents (mechanical flow).
    !     it returns rates of mass and energy flows into the layers from all mechancial vents in the building.
    !     revision: $revision: 461 $
    !     revision date: $date: 2012-02-02 14:56:39 -0500 (thu, 02 feb 2012) $

    real(eb), intent(in) :: hvpsolv(*), hvtsolv(*), tprime(*), tsec
    real(eb), intent(out) :: flwmv(mxrooms,ns+2,2), filtered(mxrooms,ns+2,2), prprime(*), deltpmv(*), delttmv(*)

    real(eb) :: filter, vheight, layer_height
    integer :: i, ii, j, k, isys, nprod, iroom
    logical :: hvacflg

    type(vent_type), pointer :: mvextptr
    type(room_type), pointer :: roomptr

    ! initialize convection coefficient for hvac ducts. ductcv is read in from solver.ini file by read_solver_ini.
    ! chv should eventually be defined elsewhere.

    hvacflg = .false.
    if (.not.mvcalc_on.or.option(fmvent)/=on.or.(nhvpvar==0.and.nhvtvar==0)) return
    hvacflg = .true.

    chv(1:nbr) = ductcv

    flwmv(1:nr,1:ns+2,u) = 0.0_eb
    flwmv(1:nr,1:ns+2,l) = 0.0_eb
    filtered(1:nr,1:ns+2,u) = 0.0_eb
    filtered(1:nr,1:ns+2,l) = 0.0_eb
    deltpmv(1:nhvpvar) = hvpsolv(1:nhvpvar)
    delttmv(1:nhvtvar) = hvtsolv(1:nhvtvar)

    call hvfrex (hvpsolv,hvtsolv)
    call hvmflo (tsec, deltpmv)
    call hvsflo (tprime,delttmv)
    call hvtoex (prprime,nprod)

    do ii = 1, n_mvext

        ! flow information for smokeview
        mvextptr => mventexinfo(ii)
        iroom = mvextptr%room
        roomptr => roominfo(iroom)
        vheight = roomptr%z0 + mvextptr%height
        layer_height = max(min(roomptr%depth(l) + roomptr%z0, vheight + sqrt(mvextptr%area)/2), vheight - sqrt(mvextptr%area)/2)
        do j = u, l
            mvextptr%temp_slab(j) = mvextptr%temp(j)
            mvextptr%flow_slab(j) = mvextptr%mv_mflow(j)
            if (j == u) then
                if (mvextptr%orientation==1) then
                    mvextptr%ybot_slab(j) = layer_height
                    mvextptr%ytop_slab(j) = vheight + sqrt(mvextptr%area)/2
                else
                    mvextptr%ybot_slab(j) = vheight
                    mvextptr%ytop_slab(j) = vheight + sqrt(mvextptr%area)/2
                end if
            else
                if (mvextptr%orientation==1) then
                    mvextptr%ybot_slab(j) = vheight - sqrt(mvextptr%area)/2
                    mvextptr%ytop_slab(j) = layer_height
                else
                    mvextptr%ybot_slab(j) = vheight - sqrt(mvextptr%area)/2
                    mvextptr%ytop_slab(j) = vheight
                end if
            end if
        end do
        mvextptr%n_slabs = 2

        i = mvextptr%room
        j = mvextptr%exterior_node
        isys = izhvsys(j)
        if (i<1.or.i>nrm1) cycle
        flwmv(i,m,u) = flwmv(i,m,u) + mvextptr%mv_mflow(u)
        flwmv(i,m,l) = flwmv(i,m,l) + mvextptr%mv_mflow(l)
        flwmv(i,q,u) = flwmv(i,q,u) + mvextptr%mv_mflow(u)*cp*mvextptr%temp(u)
        flwmv(i,q,l) = flwmv(i,q,l) + mvextptr%mv_mflow(l)*cp*mvextptr%temp(l)
        do k = 1, ns
            flwmv(i,2+k,u) = flwmv(i,2+k,u) + mvextptr%species_fraction(u,k)*mvextptr%mv_mflow(u)
            flwmv(i,2+k,l) = flwmv(i,2+k,l) + mvextptr%species_fraction(l,k)*mvextptr%mv_mflow(l)
        end do
        !	filter 9 and 11, (2+k)) = 11 and 13, smoke and radiological fraction. note that
        !   filtering is always negative. same as agglomeration and settling
        filter = qcifraction(qcvf,isys,tsec)
        filtered(i,13,u) = filtered(i,13,u) + max(0.0_eb,filter*mvextptr%species_fraction(u,11)*mvextptr%mv_mflow(u))
        filtered(i,13,l) = filtered(i,13,l) + max(0.0_eb,filter*mvextptr%species_fraction(l,11)*mvextptr%mv_mflow(l))
        filtered(i,11,u) = filtered(i,11,u) + max(0.0_eb,filter*mvextptr%species_fraction(u,9)*mvextptr%mv_mflow(u))
        filtered(i,11,l) = filtered(i,11,l) + max(0.0_eb,filter*mvextptr%species_fraction(l,9)*mvextptr%mv_mflow(l))
        !   remove filtered smoke mass and energy from the total mass and energy added to the system (likely a small effect)
        filtered(i,m,u) = filtered(i,m,u) + max(0.0_eb,filter*mvextptr%species_fraction(u,9)*mvextptr%mv_mflow(u))
        filtered(i,m,l) = filtered(i,m,l) + max(0.0_eb,filter*mvextptr%species_fraction(l,9)*mvextptr%mv_mflow(l))
        filtered(i,q,u) = filtered(i,q,u) + &
            max(0.0_eb,filter*mvextptr%species_fraction(u,9)*mvextptr%mv_mflow(u)*cp*mvextptr%temp(u))
        filtered(i,q,l) = filtered(i,q,l) + &
            max(0.0_eb,filter*mvextptr%species_fraction(l,9)*mvextptr%mv_mflow(l)*cp*mvextptr%temp(l))
    end do

    return
    end subroutine mechanical_flow

! --------------------------- hvmflo -------------------------------------------

    subroutine hvmflo (tsec, deltpmv)

    !     routine: hvmflo
    !     purpose: mass flow solution and calculation for mechanical vents
    !     arguments: tsec
    !                deltpmv

    real(eb), intent(in) :: tsec
    real(eb), intent(out) :: deltpmv(*)

    real(eb) :: pav, xtemp, f, dp

    integer :: ib, niter, iter, i, ii, j, k

    ! calculate average temperatures and densities for each branch
    pav = pressure_offset
    rohb(1:nbr) = pav/(rgas*tbr(1:nbr))
    bflo(nbr) = 1.0_eb

    ! start the iteration cycle
    niter = 2
    do iter = 1, niter

        ! initialize conductance
        ce(1:nbr)=0.0_eb

        ! convert from pressure to mass flow rate coefficients
        do ib = 1, nbr
            if (ce(ib)/=0.0_eb) then
                xtemp = 1.0_eb/sqrt(abs(ce(ib)))
                ce(ib) = sign(xtemp, ce(ib))
            end if
        end do

        ! calculate hydrostatic pressure difference terms
        do i = 1, n_mvnodes
            do j = 1, ncnode(i)
                dpz(i,j) = rohb(icmv(i,j))*grav_con*(hvght(mvintnode(i,j)) - hvght(i))
            end do
        end do

        ! find mass flow for each branch and mass residual at each node
        do i = 1, n_mvnodes
            f = 0.0_eb
            do j = 1, ncnode(i)
                dp = mv_relp(mvintnode(i,j)) - mv_relp(i) + dpz(i,j)
                if (nf(icmv(i,j))==0) then

                    ! resistive branch connection
                    hvflow(i,j) = sign(ce(icmv(i,j))*sqrt(abs(dp)), dp)
                    bflo(icmv(i,j)) = abs(hvflow(i,j))
                else

                    ! fan branch connection

                    k = nf(icmv(i,j))
                    if (ne(icmv(i,j)) /= i) then
                        ! flow is at fan inlet
                        hvflow(i,j) = -hvfan(tsec,i,j,k,dp)
                    else
                        ! flow is at fan exit
                        dp = -dp
                        hvflow(i,j) = hvfan(tsec,i,j,k,dp)
                    end if
                end if
                f = f + hvflow(i,j)
                ii = izhvie(mvintnode(i,j))
                if (ii/=0)hvflow(mvintnode(i,j),1) = -hvflow(i,j)
            end do
            ii = izhvmape(i)
            if (ii>0) deltpmv(ii) = f
        end do
    end do

    return
    end subroutine hvmflo

! --------------------------- hvsflo -------------------------------------------

    subroutine hvsflo (tprime, delttmv)

    !     routine: hvsflo
    !     purpose: species calculation for mechanical vents

    real(eb), intent(in) :: tprime(*)
    real(eb), intent(out) :: delttmv(*)

    real(eb) :: hvta, flowin, hvtemp
    integer ib, i, ii, j
    type(vent_type), pointer :: mvextptr

    delttmv(1:nbr) = rohb(1:nbr)*hvdvol(1:nbr)*tprime(1:nbr)/gamma

    do i = 1, n_mvnodes

        ! calculate temperatures & smoke flows in following loop at the connecting nodes
        hvta = 0.0_eb
        flowin = 0.0_eb
        do j = 1, ncnode(i)
            if (hvflow(i,j)>0.0_eb) then
                flowin = flowin + hvflow(i,j)
                ib = icmv(i,j)
                hvtemp = hvflow(i,j)
                hvta = hvta + hvtemp*tbr(ib)
            end if
        end do
        if (flowin>0.0_eb) then
            hvta = hvta/flowin
        else

            ! this is a bad situation.  we have no flow, yet must calculate the inflow concentrations.
            hvta = tbr(1)
            do ii = 1, n_mvext
                mvextptr => mventexinfo(ii)
                if (mvextptr%exterior_node==i) then
                    hvta = mvextptr%temp(u)
                    exit
                end if
            end do
        end if

        ! now calculate the resulting temperature and concentrations in the ducts and fans
        do j = 1, ncnode(i)
            if (hvflow(i,j)<0.0_eb) then
                ib = icmv (i,j)
                delttmv(ib) = delttmv(ib) - (hvta-tbr(ib))*abs(hvflow(i,j))
                if (option(fhvloss)==on) then
                    delttmv(ib) = delttmv(ib) + chv(ib)*(tbr(ib)-interior_temperature)*hvdara(ib)
                end if
            end if
            ii = izhvie(mvintnode(i,j))
            if (ii/=0.and.hvflow(i,j)>0.0_eb) then
                ib = icmv(i,j)
                hvta = mvextptr%temp(u)
                delttmv(ib) = delttmv(ib) - (hvta-tbr(ib))*hvflow(i,j)
                if (option(fhvloss)==on) then
                    delttmv(ib) = delttmv(ib) + chv(ib)*(tbr(ib)-interior_temperature)*hvdara(ib)
                end if
            end if
        end do
    end do
    return
    end subroutine hvsflo

! --------------------------- hvfan -------------------------------------------

    real(eb) function hvfan(tsec,i,j,k,dp)

    !     routine: hvfan
    !     purpose: calculates mass flow through a fan. this function has been modified to prevent negative flow.
    !              a max function was inserted just after the calculation off, the flow to do this.  if the flow is
    !              allowed to be negative (flow reversal) then this statement must be removed. !lso, there is now a flow
    !              restriction on the fan, using qcmfraction
    !     arguments: tsec   current simulation time
    !                i      node number
    !                j      jj'th connection to node ii
    !                k      fan number
    !                dp     head pressure across the fan

    real(eb), intent(in) :: tsec, dp
    integer, intent(in) :: i,j,k

    real(eb) :: hvfanl, openfraction, minimumopen, roh, f
    logical :: firstc = .true.
    save firstc, minimumopen

    roh = rohb(icmv(i,j))

    if (firstc) then
        minimumopen = sqrt(d1mach(1))
        firstc = .false.
    end if

    ! the hyperbolic tangent allows for smooth transition from full flow to no flow within the fan cuttoff pressure range
    f = 0.5_eb - tanh(8.0_eb/(hmax(k)-hmin(k))*(dp-hmin(k))-4.0_eb)/2.0_eb
    hvfanl = max(minimumopen, f*qmax(k)*roh)
    openfraction = max (minimumopen, qcffraction (qcvm, k, tsec))
    hvfan = hvfanl*openfraction
    return

    end function hvfan

! --------------------------- hvfrex -------------------------------------------

    subroutine hvfrex (hvpsolv, hvtsolv)

    !     routine: hvfrex
    !     purpose: update arrays and assign compartment pressures, temperatures and concentrations to flow
    !              into the system from exterior nodes

    real(eb), intent(in) :: hvpsolv(*), hvtsolv(*)

    real(eb) :: z, xxlower, xxlower_clamped, fraction, hl, hu, rhol, rhou
    integer :: i, ii, j
    type(room_type), pointer :: roomptr
    type(vent_type), pointer :: mvextptr

    do ii = 1, n_mvext
        mvextptr => mventexinfo(ii)
        i = mvextptr%room
        roomptr => roominfo(i)
        z = roomptr%depth(l)
        j = mvextptr%exterior_node
        if (mvextptr%orientation==1) then

            ! we have an opening which is oriented vertically - use a smooth crossover. first, calculate
            ! the scaling length of the duct
            xxlower = sqrt(mvextptr%area)
        else
            xxlower = sqrt(mvextptr%area)/10.0_eb
        end if

        ! then the bottom of the vent (above the floor)
        xxlower_clamped = max(0.0_eb,min((mvextptr%height - 0.5_eb*xxlower),(roomptr%cheight-xxlower)))

        ! these are the relative fraction of the upper and lower layer that the duct "sees" these parameters go from 0 to 1
        fraction = max(0.0_eb,min(1.0_eb,max(0.0_eb,(z-xxlower_clamped)/xxlower)))
        mvextptr%flow_fraction(u) = min(1.0_eb,max(1.0_eb-fraction,0.0_eb))
        mvextptr%flow_fraction(l) = min(1.0_eb,max(fraction,0.0_eb))
    end do

    ! this is the actual duct initialization
    do ii = 1, n_mvext
        mvextptr => mventexinfo(ii)
        i = mvextptr%room
        j = mvextptr%exterior_node
        if (i<nr) then
            roomptr => roominfo(i)
            z = roomptr%depth(l)
            hl = min(z,mvextptr%height)
            hu = min(0.0_eb,mvextptr%height-hl)
            rhou = roomptr%rho(u)
            rhol = roomptr%rho(l)
            mv_relp(j) = roomptr%relp - (rhol*grav_con*hl + rhou*grav_con*hu)
            mvextptr%temp(u) = roomptr%temp(u)
            mvextptr%temp(l) = roomptr%temp(l)
        else
            mvextptr%temp(u) = exterior_temperature
            mvextptr%temp(l) = exterior_temperature
            mv_relp(j) =  exterior_abs_pressure - exterior_rho*grav_con*mvextptr%height
        end if
        if (i<nr) then
            mvextptr%species_fraction(u,1:ns) = roomptr%species_fraction(u,1:ns)
            mvextptr%species_fraction(l,1:ns) = roomptr%species_fraction(l,1:ns)
        else
            mvextptr%species_fraction(u,1:ns) = initial_mass_fraction(1:ns)*exterior_rho
            mvextptr%species_fraction(l,1:ns) = initial_mass_fraction(1:ns)*exterior_rho
        end if
    end do
    do i = 1, nhvpvar
        ii = izhvmapi(i)
        mv_relp(ii) = hvpsolv(i)
    end do

    tbr(1:nhvtvar) = hvtsolv(1:nhvtvar)
    return

    end subroutine hvfrex


! --------------------------- hvtoex -------------------------------------------

    subroutine hvtoex(prprime,nprod)

    !     routine: hvfrex
    !     purpose: assign results of hvac simulation to the transfer variables (...%temp, ...%species_fraction)
    !     arguments: tsec   current simulation time
    !                prprime
    !                nprod

    real(eb), intent(out) :: prprime(*)
    integer, intent(in) :: nprod

    integer ii, j, k, ib, isys, isof, nhvpr
    type(vent_type), pointer :: mvextptr

    ! sum product flows entering system
    nhvpr = n_species*nhvsys
    if (nprod/=0) then
        prprime(1:nhvpr) = 0.0_eb
    end if
    if (ns>0) then
        hvmfsys(1:nhvsys) = 0.0_eb
        dhvprsys(1:nhvsys,1:ns) = 0.0_eb
    end if

    ! flow into the isys system
    do ii = 1, n_mvext
        mvextptr => mventexinfo(ii)
        j = mvextptr%exterior_node
        ib = icmv(j,1)
        mvextptr%mv_mflow(u) = hvflow(j,1)*mvextptr%flow_fraction(u)
        mvextptr%mv_mflow(l) = hvflow(j,1)*mvextptr%flow_fraction(l)
        isys = izhvsys(j)
        if (hvflow(j,1)<0.0_eb) then
            hvmfsys(isys) = hvmfsys(isys) + hvflow(j,1)
            if (nprod/=0) then
                dhvprsys(isys,1:ns) = dhvprsys(isys,1:ns) + abs(mvextptr%mv_mflow(u))*mvextptr%species_fraction(u,1:ns) + &
                    abs(mvextptr%mv_mflow(l))*mvextptr%species_fraction(l,1:ns)
            end if
        end if
    end do

    ! flow out of the isys system
    if (nprod/=0) then
        do k = 1, min(ns,9)
            do isys = 1, nhvsys
                if (zzhvm(isys)/=0.0_eb) then
                    dhvprsys(isys,k) = dhvprsys(isys,k) - abs(hvmfsys(isys))*zzhvspec(isys,k)/zzhvm(isys)
                end if
            end do
        end do

        ! do a special case for the non-reacting gas(es)
        k = 11
        do isys = 1, nhvsys
            if (zzhvm(isys)/=0.0_eb) then
                dhvprsys(isys,k) = dhvprsys(isys,k) - abs(hvmfsys(isys))*zzhvspec(isys,k)/zzhvm(isys)
            end if
        end do

        ! pack the species change for dassl (actually calculate_residuals)
        isof = 0
        do k = 1, min(ns,9)
            do isys = 1, nhvsys
                isof = isof + 1
                if (zzhvm(isys)/=0.0_eb) then
                    prprime(isof) = dhvprsys(isys,k)
                else
                    prprime(isof) = 0.0_eb
                end if
            end do
        end do
        ! do a special case for the non-reacting gas(es)
        k = 11
        do isys = 1, nhvsys
            isof = isof + 1
            if (zzhvm(isys)/=0.0_eb) then
                prprime(isof) = dhvprsys(isys,k)
            else
                prprime(isof) = 0.0_eb
            end if
        end do
    end if

    ! define flows or temperature leaving system
    do ii = 1, n_mvext
        mvextptr => mventexinfo(ii)
        j = mvextptr%exterior_node
        isys = izhvsys(j)
        ! we allow only one connection from a node to an external duct
        ib = icmv(j,1)
        if (hvflow(j,1)>0.0_eb) then
            mvextptr%temp(u) = tbr(ib)
            mvextptr%temp(l) = tbr(ib)
            if (zzhvm(isys)/=0.0_eb) then
                ! case 1 - finite volume and finite mass in the isys mechanical ventilation system
                mvextptr%species_fraction(u,1:ns) = zzhvspec(isys,1:ns)/zzhvm(isys)
                mvextptr%species_fraction(l,1:ns) = mvextptr%species_fraction(u,1:ns)
            else if (hvmfsys(isys)/=0.0_eb) then
                ! case 2 - zero volume (no duct). flow through the system is mdot(product)/mdot(total mass)
                mvextptr%species_fraction(u,1:ns) = -(dhvprsys(isys,1:ns)/hvmfsys(isys))
                mvextptr%species_fraction(l,1:ns) = mvextptr%species_fraction(u,1:ns)
            else
                ! case 3 - no volume and no flow = no species
                mvextptr%species_fraction(u,1:ns) = 0.0_eb
                mvextptr%species_fraction(l,1:ns) = 0.0_eb
            end if
        end if
    end do
    return
    end subroutine hvtoex

    subroutine getmventinfo (i, iroom, xyz, vred, vgreen, vblue)

    !       This is a routine to get the shape data for mechanical flow vent external connections

    integer, intent(in) :: i
    integer, intent(out) :: iroom
    real(eb), intent(out) :: xyz(6),vred,vgreen,vblue

    real(eb) :: vheight, varea
    type(room_type), pointer :: roomptr
    type(vent_type), pointer :: mvextptr

    mvextptr => mventexinfo(i)
    iroom = mvextptr%room
    roomptr => roominfo(iroom)

    vheight = mvextptr%height
    varea = mvextptr%area
    if (mvextptr%orientation==1) then
        xyz(1) = 0.0_eb
        xyz(2) = 0.0_eb
        xyz(3) = roomptr%cdepth/2 - sqrt(varea)/2
        xyz(4) = roomptr%cdepth/2 + sqrt(varea)/2
        xyz(5) = vheight - sqrt(varea)/2
        xyz(6) = vheight + sqrt(varea)/2
    else
        xyz(1) = roomptr%cwidth/2 - sqrt(varea)/2
        xyz(2) = roomptr%cwidth/2 + sqrt(varea)/2
        xyz(3) = roomptr%cdepth/2 - sqrt(varea)/2
        xyz(4) = roomptr%cdepth/2 + sqrt(varea)/2
        xyz(5) = vheight
        xyz(6) = vheight
    end if

    vred = 1.0_eb
    vgreen = 1.0_eb
    vblue = 1.0_eb

    return

    end subroutine getmventinfo

end module mflow_routines
