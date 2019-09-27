module vflow_routines

    use precision_parameters

    use opening_fractions, only: get_vent_opening
    use utility_routines, only: tanhsmooth
    
    use cfast_types, only: room_type, vent_type
    
    use cenviro, only: cp
    use cparams, only: u, l, m, q, deltatemp_min, pp, mxrooms
    use option_data, only: fvflow, option, on
    use room_data, only: nr, nrm1, ns, roominfo, exterior_ambient_temperature, interior_ambient_temperature, exterior_rho
    use vent_data, only: n_vvents, vventinfo

    implicit none

    private

    public vertical_flow

    contains

! --------------------------- vertical_flow -------------------------------------------

    subroutine vertical_flow (tsec,epsp,uflw_vf)

    ! interface between cfast and the vertical vent physical routines.
    ! arguments: tsec    current simulation time
    !            epsp    pressure error tolerance
    !            uflw_vf change in mass and energy for each layer of each compartment via flow through vertical vents

    real(eb), intent(in) :: tsec, epsp
    real(eb), intent(out) :: uflw_vf(mxrooms,ns+2,2)

    real(eb) :: vvent(2), xmvent(2), tmvent(2), frommu, fromml, fromqu, fromql, from_temp, fromtq, fl, fu
    real(eb) :: tomu, toml, toqu, toql, speciesl, speciesu, pmtoup, pmtolp
    integer ::  ilay, i, itop, ibot, iflow, ifrm, ito, lsp, index, ishape, icount
    real(eb) :: area, fraction, froude(2), alpha, zlayer, temp_upper, temp_lower
    
    character(64) :: rampid

    type(vent_type), pointer :: ventptr
    type(room_type), pointer :: roomptr

    uflw_vf(1:nr,1:ns+2,u) = 0.0_eb
    uflw_vf(1:nr,1:ns+2,l) = 0.0_eb
    if (option(fvflow)/=on) return
    if (n_vvents==0) return

    do i = 1, n_vvents
        ventptr => vventinfo(i)
        ventptr%mflow(1:2,1:2) = 0.0_eb
        itop = ventptr%room1
        ibot = ventptr%room2
        icount = ventptr%counter
        rampid = ventptr%ramp_id
        call get_vent_opening (rampid,'V',itop,ibot,icount,i,tsec,fraction)
        area = fraction * ventptr%area
        ventptr%opening_fraction = fraction
        ventptr%current_area = area
        ishape = ventptr%shape
        call ventcf (itop, ibot, area, ishape, epsp, xmvent, vvent, tmvent)

        ventptr%n_slabs = 2
        do iflow = 1, 2
            ! flow information for smokeview is relative to top room
            ventptr%temp_slab(iflow) = tmvent(iflow)
            ventptr%flow_slab(iflow) = xmvent(iflow)
            ventptr%ybot_slab(iflow) = max(0.0_eb,(ventptr%area - sqrt(area))/2.0_eb)
            ventptr%ytop_slab(iflow) = min(ventptr%area,(ventptr%area + sqrt(area))/2.0_eb)
        end do

        ! set up flow variables for DAE solver
        do iflow = 1, 2
            ! determine room where flow comes and goes
            if (iflow==1) then
                ifrm = ibot
                ito = itop
                ilay = u
            else
                ifrm = itop
                ito = ibot
                ilay = l
            end if

            ! determine mass and enthalpy fractions for the from room
            if (ifrm<=nrm1) then
                roomptr => roominfo(ifrm)
                if (tmvent(iflow)>interior_ambient_temperature) then
                    zlayer = roomptr%depth(ilay)
                    froude(iflow) = vvent(iflow) / &
                        sqrt(grav_con*zlayer**5*(tmvent(iflow)-interior_ambient_temperature)/interior_ambient_temperature)
                else
                    froude(iflow) = 0.0_eb
                end if
                alpha = exp(-(froude(iflow)/2)**2)
                if (ilay==u) then
                    ! the hyperbolic tangent allows for smooth transition to make sure we don't take from a non-existent layer
                    fu = min(tanhsmooth(roomptr%volume(u), 3.0_eb*roomptr%vmin, &
                        2.0_eb*roomptr%vmin, alpha, 0.0_eb), 1.0_eb)
                    fu = min(tanhsmooth(roomptr%volume(l), 3.0_eb*roomptr%vmin, &
                        2.0_eb*roomptr%vmin, fu, 1.0_eb), 1.0_eb)
                    fl = max(1.0_eb-fu, 0.0_eb)
                else
                    fl = min(tanhsmooth(roomptr%volume(l), 3.0_eb*roomptr%vmin, &
                        2.0_eb*roomptr%vmin, alpha, 0.0_eb), 1.0_eb)
                    fl = min(tanhsmooth(roomptr%volume(u), 3.0_eb*roomptr%vmin, &
                        2.0_eb*roomptr%vmin, fl, 1.0_eb), 1.0_eb)
                    fu = max(1.0_eb-fl, 0.0_eb)
                end if
                frommu = fu*xmvent(iflow)
                fromml = fl*xmvent(iflow)
                fromqu = cp*frommu*roomptr%temp(u)
                fromql = cp*fromml*roomptr%temp(l)
                from_temp = fu*roomptr%temp(u) + fl*roomptr%temp(l)
            else
                frommu = 0.0_eb
                fromml = xmvent(iflow)
                fromqu = 0.0_eb
                fromql = cp*fromml*exterior_ambient_temperature
                from_temp = exterior_ambient_temperature
            end if
            fromtq = fromqu + fromql

            ! extract mass and enthalpy from "from" room (not from outside)
            if (ifrm<=nrm1) then
                uflw_vf(ifrm,m,u) = uflw_vf(ifrm,m,u) - frommu
                uflw_vf(ifrm,m,l) = uflw_vf(ifrm,m,l) - fromml
                uflw_vf(ifrm,q,u) = uflw_vf(ifrm,q,u) - fromqu
                uflw_vf(ifrm,q,l) = uflw_vf(ifrm,q,l) - fromql
            end if
            ventptr%mflow(iflow,u) = ventptr%mflow(iflow,u) - frommu
            ventptr%mflow(iflow,l) = ventptr%mflow(iflow,l) - fromml

            ! determine mass and enthalpy fractions for the to room
            roomptr => roominfo(ito)
            temp_upper = roomptr%temp(u)
            temp_lower = roomptr%temp(l)
            fu = 0.0_eb
            if (from_temp>temp_lower+deltatemp_min) fu = 1.0_eb
            fl = 1.0_eb - fu
            tomu = fu*xmvent(iflow)
            toml = fl*xmvent(iflow)
            toqu = fu*fromtq
            toql = fl*fromtq

            ! deposit mass and enthalpy into "to" room varibles (not outside)
            if (ito<=nrm1) then
                uflw_vf(ito,m,u) = uflw_vf(ito,m,u) + tomu
                uflw_vf(ito,m,l) = uflw_vf(ito,m,l) + toml
                uflw_vf(ito,q,u) = uflw_vf(ito,q,u) + toqu
                uflw_vf(ito,q,l) = uflw_vf(ito,q,l) + toql
            end if
                ventptr%mflow(3-iflow,u) = ventptr%mflow(3-iflow,u) + tomu
                ventptr%mflow(3-iflow,l) = ventptr%mflow(3-iflow,l) + toml

            ! species transfer for vertical vents
            roomptr => roominfo(ifrm)
            do lsp = 1, ns
                index = pp+lsp-1
                speciesl = roomptr%species_fraction(l,lsp)*fromml
                speciesu = roomptr%species_fraction(u,lsp)*frommu

                ! extract mass and enthalpy from "from" room (not from the outside)
                if (ifrm<=nrm1) then
                    uflw_vf(ifrm,index,u) = uflw_vf(ifrm,index,u) - speciesu
                    uflw_vf(ifrm,index,l) = uflw_vf(ifrm,index,l) - speciesl
                end if

                ! deposit mass and enthalphy into "to" room variables (not outside)
                if (ito<=nrm1) then
                    pmtoup = (speciesu + speciesl)*fu
                    pmtolp = (speciesu + speciesl)*fl
                    uflw_vf(ito,index,u) = uflw_vf(ito,index,u) + pmtoup
                    uflw_vf(ito,index,l) = uflw_vf(ito,index,l) + pmtolp
                end if
            end do
        end do
    end do

    return
    end subroutine vertical_flow

! --------------------------- ventcf -------------------------------------------

    subroutine ventcf (itop, ibot, avent, nshape, epsp, xmvent, vvent, tmvent)

    ! calculates the flow of mass, enthalpy, and products of combustion through a horizontal vent joining
    ! an upper space 1 to a lower space 2. the subroutine uses input data describing the two-layer environment of
    ! inside rooms and the uniform environment in outside spaces.
    ! arguments: itop: top room number (physically with respect to the second compartment)
    !            ibot: bottom room number
    !            avent: area of the vent [m**2]
    !            nshape: number characterizing vent shape: 1 = circle, 2 = square
    !            epsp: error tolerance for dpref [dimensionless]
    !            vvent(i)    i = 1, velocity of flow from room ibot to room itop
    !                        i = 2, velocity of flow from toom itop to room ibot
    !            xmvent(i)   i = 1, mass flow from room ibot to room itop
    !                        i = 2, mass flow from room itop to room ibot
    !            tmvent(i)   i = 1, temperature in layer next to vent in top room
    !                        i = 2, temperature in layer next to vent in bottom room

    integer, intent(in) :: itop, ibot, nshape
    real(eb), intent(in) :: avent, epsp
    real(eb), intent(out) :: vvent(2), xmvent(2), tmvent(2)

    real(eb) :: den(2), relp(2), denvnt(2), dp(2), vst(2)
    integer ::  iroom(2), ilay(2)

    real(eb) :: delp, delden, rho, epscut, srdelp, fnoise
    real(eb) :: v, cshape, d, delpflood, vex
    integer :: i, deadtop, deadbot
    type(room_type), pointer :: roomptr, toproomptr, botroomptr

    toproomptr => roominfo(itop)
    botroomptr => roominfo(ibot)
    ! calculate delp, the other properties adjacent to the two sides of the vent, and delden.
    ! dp at top of bottom room and bottom of top room
    if (ibot<=nrm1) then
        dp(2) = -grav_con*(botroomptr%rho(l)*botroomptr%depth(l)+botroomptr%rho(u)*botroomptr%depth(u))
        relp(2) = botroomptr%relp
    else
        dp(2) = 0.0_eb
        relp(2) = toproomptr%exterior_relp_initial
    end if

    if (itop<=nrm1) then
        dp(1) = 0.0_eb
        relp(1) = toproomptr%relp
    else
        dp(1) = -grav_con*botroomptr%cheight*exterior_rho
        relp(1) = botroomptr%exterior_relp_initial
    end if

    ! delp is pressure immediately below the vent less pressure immediately above the vent.
    delp = relp(2) + dp(2) - (relp(1)+dp(1))

    ! if the room above or the room below is dead then  there is no pressure difference at vent opening
    deadtop = roominfo(itop)%deadroom
    deadbot = roominfo(ibot)%deadroom
    if (deadtop.ne.0.and.deadbot.ne.0.and.deadtop.eq.ibot.or.deadbot.eq.itop) delp=0.0_eb

    ! ilay(1) contains layer index in top room that is adjacent to vent
    ! ilay(2) contains layer index in bottom room that is adjacent to vent
    if (toproomptr%volume(l)<=2.0_eb*toproomptr%vmin) then
        ilay(1) = u
    else
        ilay(1) = l
    end if
    if (botroomptr%volume(u)<=2.0_eb*botroomptr%vmin) then
        ilay(2) = l
    else
        ilay(2) = u
    end if

    ! delden is density immediately above the vent less density immediately below the vent
    if (itop<=nrm1) then
        den(1) = toproomptr%rho(ilay(1))
    else
        den(1) = exterior_rho
    end if
    if (ibot<=nrm1) then
        den(2) = botroomptr%rho(ilay(2))
    else
        den(2) = exterior_rho
    end if
    delden = den(1) - den(2)

    if (delp>=0.0_eb) then
        rho = den(2)
    else
        rho = den(1)
    end if

    ! calculate factor to dampen very small flows to zero to keep dae solver happy
    epscut = epsp*max (1.0_eb, relp(1), relp(2))
    epscut = sqrt(epscut)
    srdelp = sqrt(abs(delp))
    fnoise = 1.0_eb
    if ((srdelp/epscut)<=130.0_eb) fnoise = 1.0_eb - exp(-srdelp/epscut)

    ! calculate steady flow and its direction (delp > 0, delp < 0 and delp = 0)
    v = 0.68_eb*avent*sqrt(2.0_eb*abs(delp)/rho)*fnoise

    if (delp>0.0_eb) then
        vst(1) = v
        vst(2) = 0.0_eb
    else if (delp<0.0_eb) then
        vst(1) = 0.0_eb
        vst(2) = v
    else
        vst(1) = 0.0_eb
        vst(2) = 0.0_eb
    end if

    ! calculate vex, the exchange volume rate of flow through the vent
    if (delden>0.0_eb.and.avent/=0.0_eb) then

        ! unstable configuration, calculate nonzero vex
        if (nshape==1) then
            cshape = 0.754_eb
            d = 2.0_eb*sqrt(avent/pi)
        else
            cshape = 0.942_eb
            d = sqrt(avent)
        end if
        delpflood = cshape**2*grav_con*delden*d**5/(2.0_eb*avent**2)
        vex = max(0.1_eb*sqrt(2.0_eb*grav_con*delden*sqrt(avent**5)/(den(1)+den(2)))*(1.0_eb-abs(delp/delpflood)),0.0_eb)
    else

        ! stable configuration, set vex = 0
        vex = 0.0_eb
    end if

    ! calculate the density of gas flowing through the vent
    denvnt(1) = den(2)
    denvnt(2) = den(1)

    ! calculate the vent flow rates, vvent(i), the volume flow rate through the vent into space i
    !                                xmvent(i), the mass flow rate through the vent into space i
    iroom(1) = ibot
    iroom(2) = itop
    do i = 1, 2
        vvent(i) = vst(i) + vex
        xmvent(i) = denvnt(i)*vvent(i)
        if (iroom(i)<=nrm1) then
            ! iroom(i) is an inside room so use the appropriate layer temperature
            roomptr => roominfo(iroom(i))
            tmvent(i) = roomptr%temp(ilay(3-i))
        else
            ! iroom(i) is an outside room so use exterior_ambient_temperature for temperature
            tmvent(i) = exterior_ambient_temperature
        end if
    end do
    return
    end subroutine ventcf

end module vflow_routines
