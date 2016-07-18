module mflow_routines

    use precision_parameters

    use opening_fractions, only : qcffraction, qcifraction, get_vent_opening
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

    subroutine mechanical_flow (tsec, epsp, uflw_mf, uflw_filtered)

    !     physical interface routine to calculate flow through all forced vents (mechanical flow).
    !     it returns rates of mass and energy flows into the layers from all mechancial vents in the simulation.

    real(eb), intent(in) :: tsec, epsp
    real(eb), intent(out) :: uflw_mf(mxrooms,ns+2,2), uflw_filtered(mxrooms,ns+2,2)

    real(eb) :: filter, vheight, layer_height, hvfan, fraction, fu, fl, uflw_totals(2+ns)
    integer :: i, j, k, iroom

    type(vent_type), pointer :: ventptr
    type(room_type), pointer :: roomptr

    uflw_mf(1:nr,1:ns+2,u) = 0.0_eb
    uflw_mf(1:nr,1:ns+2,l) = 0.0_eb
    uflw_filtered(1:nr,1:ns+2,u) = 0.0_eb
    uflw_filtered(1:nr,1:ns+2,l) = 0.0_eb
    if (n_mvents==0) return
    
    if (tsec>10._eb) then
        continue
    end if

    do i = 1, n_mvents
        ventptr => mventinfo(i)
        ventptr%mflow(1:2,1:2) = 0.0_eb
        uflw_totals = 0.0_eb

        ! calculate volume flow through fan
        call get_vent_opening ('M',ventptr%room1,ventptr%room2,ventptr%counter,i,tsec,fraction)
        ventptr%relp = mv_pressure(ventptr%room2,ventptr%height(2)) - mv_pressure(ventptr%room1,ventptr%height(1))
        hvfan = mv_fan(ventptr, epsp, fraction)

        ! calculate mass and enthapy flows for the from room
        iroom = ventptr%room1
        fu = mv_fraction(ventptr, 1, u)
        fl = 1.0_eb - fu
        roomptr => roominfo(iroom)
        ventptr%mflow(1,u) = fu*hvfan*roomptr%rho(u)
        ventptr%mflow(1,l) = fl*hvfan*roomptr%rho(l)
        uflw_totals(m) = ventptr%mflow(1,u) + ventptr%mflow(1,l)
        uflw_totals(q) = ventptr%mflow(1,u)*cp*roomptr%temp(u) + ventptr%mflow(1,l)*cp*roomptr%temp(l)

        if (iroom<1.or.iroom>nrm1) cycle
        uflw_mf(iroom,m,u) = uflw_mf(iroom,m,u) - ventptr%mflow(1,u)
        uflw_mf(iroom,m,l) = uflw_mf(iroom,m,l) - ventptr%mflow(1,l)
        uflw_mf(iroom,q,u) = uflw_mf(iroom,q,u) - ventptr%mflow(1,u)*cp*roomptr%temp(u)
        uflw_mf(iroom,q,l) = uflw_mf(iroom,q,l) - ventptr%mflow(1,l)*cp*roomptr%temp(l)
        do k = 1, ns
            uflw_mf(iroom,2+k,u) = uflw_mf(iroom,2+k,u) - roomptr%species_fraction(u,k)*ventptr%mflow(1,u)
            uflw_mf(iroom,2+k,l) = uflw_mf(iroom,2+k,l) - roomptr%species_fraction(l,k)*ventptr%mflow(1,l)
            uflw_totals(2+k) = roomptr%species_fraction(u,k)*ventptr%mflow(1,u) + roomptr%species_fraction(l,k)*ventptr%mflow(1,l)
        end do
        
        ! calculate mass and enthapy flows for the to room
        iroom = ventptr%room2
        fu = mv_fraction(ventptr, 2, u)
        fl = 1.0_eb - fu
        ventptr%mflow(2,u) = fu*uflw_totals(m)
        ventptr%mflow(2,l) = fl*uflw_totals(m)

        if (iroom<1.or.iroom>nrm1) cycle
        uflw_mf(iroom,m,u) = uflw_mf(iroom,m,u) + fu*uflw_totals(m)
        uflw_mf(iroom,m,l) = uflw_mf(iroom,m,l) + fl*uflw_totals(m)
        uflw_mf(iroom,q,u) = uflw_mf(iroom,q,u) + fu*uflw_totals(q)
        uflw_mf(iroom,q,l) = uflw_mf(iroom,q,l) + fl*uflw_totals(q)
        do k = 1, ns
            uflw_mf(iroom,2+k,u) = uflw_mf(iroom,2+k,u) + fu*uflw_totals(2+k)
            uflw_mf(iroom,2+k,l) = uflw_mf(iroom,2+k,l) + fl*uflw_totals(2+k)
        end do
 
        ! amount filtered for smoke and trace species, (2+k) = 11 and 13
        call get_vent_opening ('F',ventptr%room1,ventptr%room2,ventptr%counter,i,tsec,fraction)
        uflw_filtered(iroom,11,u) = uflw_filtered(iroom,11,u) + max(0.0_eb,filter*fu*uflw_totals(11))
        uflw_filtered(iroom,11,l) = uflw_filtered(iroom,11,l) + max(0.0_eb,filter*fl*uflw_totals(11))
        uflw_filtered(iroom,13,u) = uflw_filtered(iroom,13,u) + max(0.0_eb,filter*fu*uflw_totals(13))
        uflw_filtered(iroom,13,l) = uflw_filtered(iroom,13,l) + max(0.0_eb,filter*fu*uflw_totals(13))
        ! remove uflw_filtered smoke mass and energy from the total mass and energy added to the system (likely a small effect)
        uflw_filtered(iroom,m,u) = uflw_filtered(iroom,m,u) + max(0.0_eb,filter*fu*uflw_totals(11))
        uflw_filtered(iroom,m,l) = uflw_filtered(iroom,m,l) + max(0.0_eb,filter*fl*uflw_totals(11))
        uflw_filtered(iroom,q,u) = uflw_filtered(iroom,q,u) + max(0.0_eb,filter*fu* &
            (roomptr%species_fraction(u,9)*ventptr%mflow(1,u)*cp*roomptr%temp(u)+ &
            roomptr%species_fraction(l,9)*ventptr%mflow(1,l)*cp*roomptr%temp(l)))
        uflw_filtered(iroom,q,l) = uflw_filtered(iroom,q,l) + max(0.0_eb,filter*fl* &
            (roomptr%species_fraction(u,9)*ventptr%mflow(1,u)*cp*roomptr%temp(u)+ &
            roomptr%species_fraction(l,9)*ventptr%mflow(1,l)*cp*roomptr%temp(l)))


    end do

    do i = 1, n_mvents

        ! flow information for smokeview
        ventptr => mventinfo(i)
        iroom = ventptr%room1
        roomptr => roominfo(iroom)
        vheight = roomptr%z0 + ventptr%height(1)
        layer_height = max(min(roomptr%depth(l) + roomptr%z0, vheight + sqrt(ventptr%area)/2), vheight - sqrt(ventptr%area)/2)
        do j = u, l
            ventptr%temp_slab(j) = roomptr%temp(j)
            ventptr%flow_slab(j) = ventptr%mflow(1,j)
            if (j == u) then
                if (ventptr%orientation(1)==1) then
                    ventptr%ybot_slab(j) = layer_height
                    ventptr%ytop_slab(j) = vheight + sqrt(ventptr%area)/2
                else
                    ventptr%ybot_slab(j) = vheight
                    ventptr%ytop_slab(j) = vheight + sqrt(ventptr%area)/2
                end if
            else
                if (ventptr%orientation(1)==1) then
                    ventptr%ybot_slab(j) = vheight - sqrt(ventptr%area)/2
                    ventptr%ytop_slab(j) = layer_height
                else
                    ventptr%ybot_slab(j) = vheight - sqrt(ventptr%area)/2
                    ventptr%ytop_slab(j) = vheight
                end if
            end if
        end do
        ventptr%n_slabs = 2

    end do

    return
    end subroutine mechanical_flow

! --------------------------- mv_fan -------------------------------------------

    real(eb) function mv_fan (ventptr, epsp, fraction)

    type(vent_type), intent(in) :: ventptr
    real(eb), intent(in) :: epsp, fraction

    real(eb) :: epscut
    
    epscut = fraction*(0.5_eb - tanh(8.0_eb/(ventptr%max_cutoff_relp-ventptr%min_cutoff_relp)* &
        (ventptr%relp-ventptr%min_cutoff_relp)-4.0_eb)/2.0_eb)
    mv_fan = epscut*max(epsp,ventptr%maxflow)

    end function mv_fan

! --------------------------- mv_pressure -------------------------------------------
    
    real(eb) function mv_pressure(iroom, height)
    
    integer, intent(in) :: iroom
    real(eb), intent(in) :: height

    real(eb) :: z, hl, hu, rhol, rhou
    type(room_type), pointer :: roomptr

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

! --------------------------- mv_fraction -------------------------------------------
    
    real(eb) function mv_fraction (ventptr, ifromto, layer)
    
    integer, intent(in) :: ifromto, layer
    type(room_type), pointer :: roomptr
    type(vent_type) :: ventptr
    
    integer :: iroom
    real(eb) :: z, xxlower, xxlower_clamped, fraction
    
    if (ifromto==1) then
        iroom = ventptr%room1
    else
        iroom = ventptr%room2
    end if
    
    roomptr => roominfo(iroom)
    z = roomptr%depth(l)
    
    if (ventptr%orientation(ifromto)==1) then
        xxlower = sqrt(ventptr%diffuser_area(ifromto))
    else
        xxlower = sqrt(ventptr%diffuser_area(ifromto))/10.0_eb
    end if
    xxlower_clamped = max(0.0_eb,min((ventptr%height(ifromto) - 0.5_eb*xxlower),(roomptr%cheight-xxlower)))
    
    ! these are the relative fraction of the upper and lower layer that the duct "sees" these parameters go from 0 to 1
    fraction = max(0.0_eb,min(1.0_eb,max(0.0_eb,(z-xxlower_clamped)/xxlower)))
    if (layer==u) then
        mv_fraction = min(1.0_eb,max(1.0_eb-fraction,0.0_eb))
    else
        mv_fraction = min(1.0_eb,max(fraction,0.0_eb))
    end if
    return
    
    end function mv_fraction

! --------------------------- getmventinfo -------------------------------------------

    subroutine getmventinfo (i, iroom, xyz, vred, vgreen, vblue)

    !       This is a routine to get the shape data for mechanical flow vent external connections

    integer, intent(in) :: i
    integer, intent(out) :: iroom
    real(eb), intent(out) :: xyz(6),vred,vgreen,vblue

    real(eb) :: vheight, varea
    type(room_type), pointer :: roomptr
    type(vent_type), pointer :: ventptr

    ventptr => mventinfo(i)
    iroom = ventptr%room1
    roomptr => roominfo(iroom)

    vheight = ventptr%height(1)
    varea = ventptr%diffuser_area(1)
    if (ventptr%orientation(1)==1) then
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
