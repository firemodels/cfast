module mflow_routines

    use precision_parameters

    use opening_fractions, only : get_vent_opening
    use utility_routines, only: tanhsmooth
    
    use cfast_types, only: room_type, vent_type
    
    use cenviro, only: cp
    use cparams, only: u, l, m, q, soot, soot_flaming, soot_smolder, ts, mxrooms
    use option_data, only: fmflow, option, off
    use room_data, only: n_rooms, ns, roominfo, exterior_rho, interior_rho, exterior_abs_pressure
    use vent_data, only: n_mvents, mventinfo

    implicit none

    private

    public mechanical_flow

    contains

! --------------------------- mechanical_flow -------------------------------------------

!> \brief   physical interface routine to calculate flow through all forced vents (mechanical flow).
!>          it returns rates of mass and energy flows into the layers from all mechancial vents in the simulation.
    
!> \param   tsec (input): current simulation time (s)
!> \param   epsp (input): pressure error tolerance
!> \param   uflw_mf (output): change in mass and energy for each layer / compartment via flow through mechanical vents
!> \param   iflw_filtered (output): mass and energy removed from system via filtering at mechanical vents

    subroutine mechanical_flow (tsec, epsp, uflw_mf, uflw_filtered)

    real(eb), intent(in) :: tsec, epsp
    real(eb), intent(out) :: uflw_mf(mxrooms,ns+2,2), uflw_filtered(mxrooms,ns+2,2)

    real(eb) :: filter, vheight, layer_height, hvfan, fraction, fu, fl, uflw_totals(2+ns)
    real(eb) :: dt, dy, dydt, deltat, mintime = 1.0e-6_eb
    integer :: i, j, k, iroom

    type(vent_type), pointer :: ventptr
    type(room_type), pointer :: roomptr

    uflw_mf(1:n_rooms+1,1:ns+2,u) = 0.0_eb
    uflw_mf(1:n_rooms+1,1:ns+2,l) = 0.0_eb
    uflw_filtered(1:n_rooms+1,1:ns+2,u) = 0.0_eb
    uflw_filtered(1:n_rooms+1,1:ns+2,l) = 0.0_eb
    if (n_mvents==0) return
    if (option(fmflow)==off) return

    do i = 1, n_mvents
        ventptr => mventinfo(i)
        ventptr%mflow(1:2,1:2) = 0.0_eb
        uflw_totals = 0.0_eb

        ! calculate volume flow through fan
        call get_vent_opening (ventptr,tsec,fraction)
        ventptr%relp = mv_pressure(ventptr%room2,ventptr%height(2)) - mv_pressure(ventptr%room1,ventptr%height(1))
        ventptr%opening_fraction = fraction
        ventptr%current_area = ventptr%diffuser_area(1)*fraction
        hvfan = mv_fan(ventptr, epsp, fraction)

        ! calculate mass and enthapy flows for the from room
        iroom = ventptr%room1
        fu = mv_fraction(ventptr, 1, u)
        fl = 1.0_eb - fu
        roomptr => roominfo(iroom)
        ventptr%mflow(1,u) = -fu*hvfan*roomptr%rho(u)
        ventptr%mflow(1,l) = -fl*hvfan*roomptr%rho(l)
        
        uflw_totals(m) = -(ventptr%mflow(1,u) + ventptr%mflow(1,l))
        uflw_totals(q) = -(ventptr%mflow(1,u)*cp*roomptr%temp(u) + ventptr%mflow(1,l)*cp*roomptr%temp(l))
        do k = 1, ns
            uflw_totals(2+k) = -(roomptr%species_fraction(u,k)*ventptr%mflow(1,u) + &
                roomptr%species_fraction(l,k)*ventptr%mflow(1,l))
        end do
        
        ! calculate fraction of trace species and soot filtered out for this vent
        filter = 0._eb
        if (tsec<ventptr%filter_initial_time) then
            filter = ventptr%filter_initial_fraction
        else if (tsec>ventptr%filter_final_time) then
            filter = ventptr%filter_final_fraction
        else
            dt = max(ventptr%filter_final_time - ventptr%filter_initial_time, mintime)
            deltat = max(tsec - ventptr%filter_initial_time, mintime)
            dy = ventptr%filter_final_fraction - ventptr%filter_initial_fraction
            dydt = dy/dt
            filter = ventptr%filter_initial_fraction + dydt*deltat
        end if
        
        ! calculate species mass and enthalpy flows
        if (iroom<=n_rooms) then
            uflw_mf(iroom,m,u) = uflw_mf(iroom,m,u) + ventptr%mflow(1,u)
            uflw_mf(iroom,m,l) = uflw_mf(iroom,m,l) + ventptr%mflow(1,l)
            uflw_mf(iroom,q,u) = uflw_mf(iroom,q,u) + ventptr%mflow(1,u)*cp*roomptr%temp(u)
            uflw_mf(iroom,q,l) = uflw_mf(iroom,q,l) + ventptr%mflow(1,l)*cp*roomptr%temp(l)
            do k = 1, ns
                uflw_mf(iroom,2+k,u) = uflw_mf(iroom,2+k,u) + roomptr%species_fraction(u,k)*ventptr%mflow(1,u)
                uflw_mf(iroom,2+k,l) = uflw_mf(iroom,2+k,l) + roomptr%species_fraction(l,k)*ventptr%mflow(1,l)
                ventptr%species_fraction(u,k) = roomptr%species_fraction(u,k)
                ventptr%species_fraction(l,k) = roomptr%species_fraction(l,k)
            end do
            ! amount filtered for smoke and trace species
            uflw_filtered(iroom,soot+2,u) = uflw_filtered(iroom,soot+2,u) + max(0.0_eb,filter*fu*uflw_totals(soot+2))
            uflw_filtered(iroom,soot+2,l) = uflw_filtered(iroom,soot+2,l) + max(0.0_eb,filter*fl*uflw_totals(soot+2))
            uflw_filtered(iroom,soot_flaming+2,u) = uflw_filtered(iroom,soot_flaming+2,u) + &
                max(0.0_eb,filter*fu*uflw_totals(soot_flaming+2))
            uflw_filtered(iroom,soot_flaming+2,l) = uflw_filtered(iroom,soot_flaming+2,l) + &
                max(0.0_eb,filter*fl*uflw_totals(soot_flaming+2))
            uflw_filtered(iroom,soot_smolder+2,u) = uflw_filtered(iroom,soot_smolder+2,u) + &
                max(0.0_eb,filter*fu*uflw_totals(soot_smolder+2))
            uflw_filtered(iroom,soot_smolder+2,l) = uflw_filtered(iroom,soot_smolder+2,l) + &
                max(0.0_eb,filter*fl*uflw_totals(soot_smolder+2))
            uflw_filtered(iroom,ts+2,u) = uflw_filtered(iroom,ts+2,u) + max(0.0_eb,filter*fu*uflw_totals(ts+2))
            uflw_filtered(iroom,ts+2,l) = uflw_filtered(iroom,ts+2,l) + max(0.0_eb,filter*fl*uflw_totals(ts+2))
        end if

        ! calculate mass and enthapy flows for the to room
        iroom = ventptr%room2
        fu = mv_fraction(ventptr, 2, u)
        fl = 1.0_eb - fu
        ventptr%mflow(2,u) = fu*uflw_totals(m)
        ventptr%mflow(2,l) = fl*uflw_totals(m)

        if (iroom<=n_rooms) then
            uflw_mf(iroom,m,u) = uflw_mf(iroom,m,u) + fu*uflw_totals(m)
            uflw_mf(iroom,m,l) = uflw_mf(iroom,m,l) + fl*uflw_totals(m)
            uflw_mf(iroom,q,u) = uflw_mf(iroom,q,u) + fu*uflw_totals(q)
            uflw_mf(iroom,q,l) = uflw_mf(iroom,q,l) + fl*uflw_totals(q)
            do k = 1, ns
                uflw_mf(iroom,2+k,u) = uflw_mf(iroom,2+k,u) + fu*uflw_totals(2+k)
                uflw_mf(iroom,2+k,l) = uflw_mf(iroom,2+k,l) + fl*uflw_totals(2+k)
            end do

            ! remove uflw_filtered smoke mass and energy from the total mass and energy added to the system (likely a small effect)
            uflw_filtered(iroom,m,u) = uflw_filtered(iroom,m,u) + max(0.0_eb,filter*fu*uflw_totals(11))
            uflw_filtered(iroom,m,l) = uflw_filtered(iroom,m,l) + max(0.0_eb,filter*fl*uflw_totals(11))
            uflw_filtered(iroom,q,u) = uflw_filtered(iroom,q,u) + max(0.0_eb,filter*fu* &
                (roomptr%species_fraction(u,soot)*ventptr%mflow(1,u)*cp*roomptr%temp(u) + &
                roomptr%species_fraction(l,soot)*ventptr%mflow(1,l)*cp*roomptr%temp(l)))
            uflw_filtered(iroom,q,l) = uflw_filtered(iroom,q,l) + max(0.0_eb,filter*fl* &
                (roomptr%species_fraction(u,soot)*ventptr%mflow(1,u)*cp*roomptr%temp(u) + &
                roomptr%species_fraction(l,soot)*ventptr%mflow(1,l)*cp*roomptr%temp(l)))
        end if

        ! flow information for smokeview

        iroom = ventptr%room1
        roomptr => roominfo(iroom)
        vheight = roomptr%z0 + ventptr%height(1)
        layer_height = max(min(roomptr%depth(l) + roomptr%z0, vheight + &
            sqrt(ventptr%diffuser_area(1))/2), vheight - sqrt(ventptr%diffuser_area(1))/2)
        do j = u, l
            ventptr%temp_slab(j) = roomptr%temp(j)
            ventptr%flow_slab(j) = ventptr%mflow(1,j)
            if (j == u) then
                if (ventptr%orientation(1)==1) then
                    ventptr%ybot_slab(j) = layer_height
                    ventptr%ytop_slab(j) = vheight + sqrt(ventptr%diffuser_area(1))/2
                else
                    ventptr%ybot_slab(j) = vheight
                    ventptr%ytop_slab(j) = vheight + sqrt(ventptr%diffuser_area(1))/2
                end if
            else
                if (ventptr%orientation(1)==1) then
                    ventptr%ybot_slab(j) = vheight - sqrt(ventptr%diffuser_area(1))/2
                    ventptr%ytop_slab(j) = layer_height
                else
                    ventptr%ybot_slab(j) = vheight - sqrt(ventptr%diffuser_area(1))/2
                    ventptr%ytop_slab(j) = vheight
                end if
            end if
        end do
        ventptr%n_slabs = 2

    end do

    return
    end subroutine mechanical_flow

! --------------------------- mv_fan -------------------------------------------
    
!> \brief   calculate the fan flow in m^3/s.  at the moment, it's just a constant flow fan
    
!> \param   ventptr (input) pointer to vent of interest
!> \param   epsp (input) pressure error tolerance
!> \param   fraction (input) current vent opening fraction

    real(eb) function mv_fan (ventptr, epsp, fraction)

    type(vent_type), intent(in) :: ventptr
    real(eb), intent(in) :: epsp, fraction

    real(eb) :: epscut
    
    epscut = tanhsmooth(ventptr%relp,ventptr%min_cutoff_relp,ventptr%max_cutoff_relp,1.0_eb,0.0_eb)
    mv_fan = epscut*fraction*max(epsp,ventptr%maxflow)

    end function mv_fan

! --------------------------- mv_pressure -------------------------------------------
    
!> \brief   calculate the absolute pressure in the vented compartment at the specified height
    
!> \param   iroom (input) compartment number
!> \param   height (input) height within compartment where pressure is calculated
    
    real(eb) function mv_pressure(iroom, height)
    
    integer, intent(in) :: iroom
    real(eb), intent(in) :: height

    real(eb) :: z, hl, hu, rhol, rhou
    type(room_type), pointer :: roomptr

    if (iroom<n_rooms+1) then
        roomptr => roominfo(iroom)
        z = roomptr%depth(l)
        hl = min(z,height)
        hu = max(0.0_eb,height-hl)
        rhou = roomptr%rho(u)
        rhol = roomptr%rho(l)
        mv_pressure = roomptr%relp - interior_rho*grav_con*roomptr%z0 - (rhol*grav_con*hl + rhou*grav_con*hu)
    else
        mv_pressure =  exterior_abs_pressure - exterior_rho*grav_con*height
    end if

    end function mv_pressure

! --------------------------- mv_fraction -------------------------------------------
    
!> \brief   calculate the fraction of mechanical flow that come from or goes to a layer
    
!> \param   ventptr (input) pointer to vent of interest
!> \param   ifromto (input) 1 = flow from first compartment from specified layer
!>                          2 = flow into second compartment into specified layer
!> \param   layer (input) 1 for upper layer, 2 for lower layer
    
    real(eb) function mv_fraction (ventptr, ifromto, layer)
    
    integer, intent(in) :: ifromto, layer
    type(vent_type), intent(in) :: ventptr
    
    integer :: iroom
    real(eb) :: z, zlower, zupper, fraction, diffuser_height
    type(room_type), pointer :: roomptr
    
    if (ifromto==1) then
        iroom = ventptr%room1
    else
        iroom = ventptr%room2
    end if
    roomptr => roominfo(iroom)
    z = roomptr%depth(l)
    if (ventptr%orientation(ifromto)==1) then
        diffuser_height = sqrt(ventptr%diffuser_area(ifromto))
    else
        diffuser_height = sqrt(ventptr%diffuser_area(ifromto))/10.0_eb
    end if
    
    ! this is the distance over which the flow transistions from all lower to all upper
    zlower = max(0.0_eb,ventptr%height(ifromto)-diffuser_height/2)
    zupper = min(roomptr%cheight,ventptr%height(ifromto)+diffuser_height/2)
    
    ! transition smoothly from all lower (when to layer height is above the vent, zupper) 
    ! to all upper (when to layer height is below the vent, zlower)
    fraction = tanhsmooth (z, zupper, zlower, 1.0_eb, 0.0_eb)
    if (layer==u) then
        mv_fraction = min(1.0_eb,max(1.0_eb-fraction,0.0_eb))
    else
        mv_fraction = min(1.0_eb,max(fraction,0.0_eb))
    end if
    return
    
    end function mv_fraction

end module mflow_routines
