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

    subroutine mechanical_flow (tsec, epsp, uflw_mf, uflw_filtered, hvpsolv, hvtsolv, tprime, deltpmv, delttmv, prprime, nprod, hvacflg)

    !     physical interface routine to calculate flow through all forced vents (mechanical flow).
    !     it returns rates of mass and energy flows into the layers from all mechancial vents in the simulation.

    real(eb), intent(in) :: hvpsolv(*), hvtsolv(*), tprime(*), tsec, epsp
    real(eb), intent(out) :: uflw_mf(mxrooms,ns+2,2), uflw_filtered(mxrooms,ns+2,2), prprime(*), deltpmv(*), delttmv(*)

    real(eb) :: filter, vheight, layer_height, epscut, hvfan
    integer :: i, ii, j, k, isys, nprod, iroom
    logical :: firstc, hvacflg
    save firstc

    type(vent_type), pointer :: mvextptr
    type(room_type), pointer :: roomptr


    if (firstc) then
        minimumopen = sqrt(d1mach(1))
        firstc = .false.
    end if

    uflw_mf(1:nr,1:ns+2,u) = 0.0_eb
    uflw_mf(1:nr,1:ns+2,l) = 0.0_eb
    uflw_filtered(1:nr,1:ns+2,u) = 0.0_eb
    uflw_filtered(1:nr,1:ns+2,l) = 0.0_eb
    if (n_mvents==0) return
    
    do i = 1, n_mvents
        ventptr => mventinfo(i)
        ventptr%mflow(1:2,1:2) = 0.0_eb

        ! calculate volume flow through fan
        call getventfraction ('M',ventptr%room1,ventptr%room2,ventptr%counter,i,tsec,fraction)
        ventptr%relp = mv_pressure(ventptr%room2,ventptr%height(2)) - mv_pressure(ventptr%room1,ventptr%height(1))
        epscut = fraction*(0.5_eb - tanh(8.0_eb/(ventptr%max_cutoff_relp-ventptr%min_cutoff_relp)*&
            (ventptr%relp-ventptr%min_cutoff_relp)-4.0_eb)/2.0_eb)
        hvfan = epscut*max(minimumopen,ventptr%mv_maxflow)

        ! calculate mass and enthapy fractions for the from room
        fu = mv_fraction(ventptr, 1, upper)
        fl = 1.0_eb - fu
        roomptr => roominfo(ventptr%room1)
        ventptr%mflow(1,u) = -fu*hvfan*roomptr%rho(u)
        ventptr%mflow(1,l) = -fl*hvfan*roomptr%rho(l)

        ! calculate mass and enthapy fractions for the to room
        fu = mv_fraction(ventptr, 2, upper)
        fl = 1.0_eb - fu
        flwtotal = -(ventptr%mflow(1,u)+ventptr%mflow(1,l))
        ventptr%mflow(2,u) = -fu*flwtotal
        ventptr%mflow(2,l) = -fl*flwtotal
    end do

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
        uflw_mf(i,m,u) = uflw_mf(i,m,u) + mvextptr%mv_mflow(u)
        uflw_mf(i,m,l) = uflw_mf(i,m,l) + mvextptr%mv_mflow(l)
        uflw_mf(i,q,u) = uflw_mf(i,q,u) + mvextptr%mv_mflow(u)*cp*mvextptr%temp(u)
        uflw_mf(i,q,l) = uflw_mf(i,q,l) + mvextptr%mv_mflow(l)*cp*mvextptr%temp(l)
        do k = 1, ns
            uflw_mf(i,2+k,u) = uflw_mf(i,2+k,u) + mvextptr%species_fraction(u,k)*mvextptr%mv_mflow(u)
            uflw_mf(i,2+k,l) = uflw_mf(i,2+k,l) + mvextptr%species_fraction(l,k)*mvextptr%mv_mflow(l)
        end do
        !	filter 9 and 11, (2+k)) = 11 and 13, smoke and radiological fraction. note that
        !   filtering is always negative
        filter = qcifraction(qcvf,isys,tsec)
        uflw_filtered(i,13,u) = uflw_filtered(i,13,u) + max(0.0_eb,filter*mvextptr%species_fraction(u,11)*mvextptr%mv_mflow(u))
        uflw_filtered(i,13,l) = uflw_filtered(i,13,l) + max(0.0_eb,filter*mvextptr%species_fraction(l,11)*mvextptr%mv_mflow(l))
        uflw_filtered(i,11,u) = uflw_filtered(i,11,u) + max(0.0_eb,filter*mvextptr%species_fraction(u,9)*mvextptr%mv_mflow(u))
        uflw_filtered(i,11,l) = uflw_filtered(i,11,l) + max(0.0_eb,filter*mvextptr%species_fraction(l,9)*mvextptr%mv_mflow(l))
        !   remove uflw_filtered smoke mass and energy from the total mass and energy added to the system (likely a small effect)
        uflw_filtered(i,m,u) = uflw_filtered(i,m,u) + max(0.0_eb,filter*mvextptr%species_fraction(u,9)*mvextptr%mv_mflow(u))
        uflw_filtered(i,m,l) = uflw_filtered(i,m,l) + max(0.0_eb,filter*mvextptr%species_fraction(l,9)*mvextptr%mv_mflow(l))
        uflw_filtered(i,q,u) = uflw_filtered(i,q,u) + &
            max(0.0_eb,filter*mvextptr%species_fraction(u,9)*mvextptr%mv_mflow(u)*cp*mvextptr%temp(u))
        uflw_filtered(i,q,l) = uflw_filtered(i,q,l) + &
            max(0.0_eb,filter*mvextptr%species_fraction(l,9)*mvextptr%mv_mflow(l)*cp*mvextptr%temp(l))
    end do

    return
    end subroutine mechanical_flow

! --------------------------- mvpressure -------------------------------------------
    
    real(eb) function mv_pressure(iroom, height)
    
    integer, intent(in) :: iroom
    real(eb), intent(in) :: height
    
    integer ifromto
    real(eb) :: z, hl, hu, rhol, rhou

    if (iroom<nr) then
        roomptr => roominfo(iroom)
        z = roomptr%depth(l)
        hl = min(z,height)
        hu = min(0.0_eb,height-hl)
        rhou = roomptr%rho(u)
        rhol = roomptr%rho(l)
        mv_pressure = roomptr%relp - (rhol*grav_con*hl + rhou*grav_con*hu)
    else
        mv_pressure =  exterior_abs_pressure - exterior_rho*grav_con*height
    end if

    end function mv_pressure

! --------------------------- hvfan -------------------------------------------

    real(eb) function mvfan(tsec,i,dp)

    !     routine: hvfan
    !     purpose: calculates mass flow through a fan. this function has been modified to prevent negative flow.
    !              a max function was inserted just after the calculation off, the flow to do this.  if the flow is
    !              allowed to be negative (flow reversal) then this statement must be removed. !lso, there is now a flow
    !              restriction on the fan, using qcmfraction
    !     arguments: tsec   current simulation time
    !                i      vent number
    !                dp     head pressure across the fan

    real(eb), intent(in) :: tsec, dp
    integer, intent(in) :: i

    real(eb) :: hvfanl, openfraction, minimumopen, rho, f
    integer :: iroom
    logical :: firstc = .true.
    save firstc, minimumopen
    type(room_type), pointer :: roomptr
    type(vent_type), pointer ::  ventptr

    roomptr => roominfo(ventptr%iroom1)
    rho =roomptr%

    if (firstc) then
        minimumopen = sqrt(d1mach(1))
        firstc = .false.
    end if
    
    ! the hyperbolic tangent allows for smooth transition from full flow to no flow within the fan cuttoff pressure range
    ventptr => mventinfo(i)
    f = 0.5_eb - tanh(8.0_eb/(ventptr%max_cutoff_relp-ventptr%min_cutoff_relp)*(dp-ventptr%min_cutoff_relp)-4.0_eb)/2.0_eb
    hvfan = max(minimumopen, f*ventptr%mv_maxflow*rho)
    return

    end function mvfan

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
