module fire_routines

    use precision_parameters

    use opening_fractions, only: get_vent_opening
    use utility_routines, only: tanhsmooth, xerror, interp

    use cfast_types, only: detector_type, fire_type, room_type, target_type, vent_type
        
    use cenviro, only: cp
    use cparams, only: u, l, m, q, ts, fuel, n2, o2, o2f, mxfires, mxrooms, fuel_moles, fuel_q, fuel_n2, fuel_o2, fuel_co2, &
        fuel_co, fuel_hcn, fuel_hcl, hcl, fuel_h2o, h2o, fuel_soot, co2, co, hcn, soot, soot_flaming, soot_smolder, ct, &
        mx_hsep, t_max, ns_mass, flaming, smoldering, trigger_by_time, trigger_by_temp, trigger_by_flux, idx_tempf_trg, &
        check_state, set_state
    use fire_data, only: n_fires, fireinfo, lower_o2_limit, summed_total_trace, tgignt, sigma_s
    use option_data, only: ffire, option, fdfire, on, off
    use room_data, only: nr, nrm1, ns, roominfo, interior_ambient_temperature, adiabatic_walls
    use setup_data, only: iofill, iofilo
    use smkview_data, only: smv_room, smv_height, smv_qdot, smv_xfire, smv_yfire, smv_zfire
    use solver_data, only: atol
    use target_data, only: detectorinfo, targetinfo
    use vent_data, only: n_hvents, hventinfo, n_mvents, mventinfo, vss, vsa, vsas

    implicit none

    private

    public door_jets, fire, flame_height, get_gas_tempandvelocity, integrate_mass, collect_fire_data_for_smokeview, &
        update_fire_objects, update_species

    contains
! --------------------------- fires -------------------------------------------

    subroutine fire (tsec,flows_fires)

    ! physical interface routine to calculate the current rates of mass and energy flows into the layers from
    !   all fires in the building.
    
    ! input   tsec          current simulation time (s)
    ! output  flows_fires   mass and energy flows into layers due to fires.

    real(eb), intent(in) :: tsec
    real(eb), intent(out) :: flows_fires(mxrooms,ns+2,2)

    real(eb) :: species_mass_rate(2,ns), species_mass(2,ns), n_C, n_H, n_O, n_N, n_Cl
    real(eb) :: mdot_t, area_t, height_t, qdot_t, hoc_t, y_soot, y_co, y_hcn, y_trace, t_lower, q_firemass, &
        q_entrained, hrr_r, hrr_c, y_soot_flaming, y_soot_smolder
    integer iroom, i, nfire
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr

    flows_fires(1:nr,1:ns+2,u) = 0.0_eb
    flows_fires(1:nr,1:ns+2,l) = 0.0_eb
    nfire = 0

    if (option(ffire)==off) return

    do i = 1, n_fires
        fireptr => fireinfo(i)
        iroom = fireptr%room
        roomptr => roominfo(iroom)
        
        call interpolate_pyrolysis(i,tsec,iroom,mdot_t,area_t,height_t,qdot_t,hoc_t,n_C,n_H,n_O,n_N,n_Cl,y_soot,y_soot_flaming, &
            y_soot_smolder,y_co,y_hcn,y_trace)

        fireptr%mdot_pyrolysis = mdot_t
        fireptr%qdot_theoretical = qdot_t
        fireptr%z_offset = height_t
        species_mass(u,1:ns) = roomptr%species_mass(u,1:ns)
        species_mass(l,1:ns) = roomptr%species_mass(l,1:ns)
        
        call do_fire(i, iroom, fireptr%mdot_pyrolysis, roomptr%cheight, roomptr%cwidth, roomptr%cdepth, hoc_t, y_soot, &
            y_soot_flaming, y_soot_smolder, y_co, y_hcn, y_trace, n_C, n_H, n_O, n_N, n_Cl, fireptr%molar_mass, species_mass, &
            fireptr%x_position, fireptr%y_position, fireptr%z_position+fireptr%z_offset, area_t, fireptr%mdot_entrained, &
            fireptr%mdot_plume, qdot_t, species_mass_rate, hrr_c, hrr_r, fireptr%qdot_layers(l), fireptr%qdot_layers(u))
        
        fireptr%firearea = area_t
        fireptr%mdot_trace = fireptr%mdot_pyrolysis*y_trace
        fireptr%qdot_actual = fireptr%qdot_layers(l) + fireptr%qdot_layers(u)
        fireptr%qdot_convective = hrr_c
        fireptr%qdot_radiative = hrr_r

        ! sum the flows for return to the source routine
        t_lower = roomptr%temp(l)
        flows_fires(iroom,m,u) = flows_fires(iroom,m,u) + fireptr%mdot_plume
        flows_fires(iroom,m,l) = flows_fires(iroom,m,l) - fireptr%mdot_entrained
        q_firemass = cp*fireptr%mdot_pyrolysis*interior_ambient_temperature
        q_entrained = cp*fireptr%mdot_entrained*t_lower
        flows_fires(iroom,q,u) = flows_fires(iroom,q,u) + hrr_c + q_firemass + q_entrained
        flows_fires(iroom,q,l) = flows_fires(iroom,q,l) - q_entrained
        flows_fires(iroom,3:ns+2,u) = flows_fires(iroom,3:ns+2,u) + species_mass_rate(u,1:ns)
        flows_fires(iroom,3:ns+2,l) = flows_fires(iroom,3:ns+2,l) + species_mass_rate(l,1:ns)
    end do

    return
    end subroutine fire

! --------------------------- interpolate_pyrolysis -------------------------------------------

    subroutine interpolate_pyrolysis (ifire,tsec,iroom,mdot_t,area_t,height_t,qdot_t,hoc_t,n_C,n_H,n_O,n_N,n_Cl,y_soot, &
                                      y_soot_flaming, y_soot_smolder,y_co,y_hcn, y_trace)

    ! returns fire yields at current time interpolated from user input
    
    ! inputs   ifire                        fire pointer number
    !          tsec                         current simulation time (s)
    !          iroom                        room contining the object
    ! outputs  mdot_t                       pyrolysis rate of object
    !          area_t                       area of pyrolysis of object
    !          height_t                     height of fire
    !          qdot_t                       HRR
    !          hoc_t                        heat of combustion
    !          n_C, n_H, n_O, n_N, n_Cl     molecular formula for the fuel; these can be fractional;
    !                                       yields of O2, HCl, and HCN are determined from this
    !          y_soot, y_co, y_hcn, y_trace species yields for soot, CO, HCN, and trace species;
    !                                       others are calculated from the molecular formula of the fuel

    integer, intent(in) :: ifire, iroom
    real(eb), intent(in) :: tsec
    real(eb), intent(out) :: mdot_t, area_t, height_t, qdot_t, hoc_t, n_C, n_H, n_O, n_N, n_Cl, y_soot, y_soot_flaming 
    real(eb), intent(out) :: y_soot_smolder, y_co, y_hcn, y_trace

    real(eb) :: xxtime, tdrate, xxtimef, qt, qtf, tfact, factor, tfilter_max
    integer :: id, ifact
    
    type(room_type), pointer :: roomptr
    type(detector_type), pointer :: dtectptr
    type(fire_type), pointer :: fireptr

    roomptr => roominfo(iroom)
    fireptr => fireinfo(ifire)
    
    if (.not.fireptr%ignited) then
        mdot_t = 0.0_eb
        area_t = 0.0_eb
        height_t = 0.0_eb
        qdot_t = 0.0_eb
        n_C = 1.0_eb
        n_H = 4.0_eb
        n_O = 0.0_eb
        n_N = 0.0_eb
        n_Cl = 0.0_eb
        hoc_t = 5.0e7_eb
        y_soot = 0.0_eb
        y_soot_flaming = 0.0_eb
        y_soot_smolder = 0.0_eb
        y_co = 0.0_eb
        y_hcn = 0.0_eb
        y_trace = 0.0_eb
        return
    end if

    xxtime = tsec - fireptr%ignition_time

    id = roomptr%sprinkler_activated

    if (id==0) then
        ! if a sprinkler is not active then interpolate at current time
        ifact = 0
    else
        ! if a sprinkler is active then interpolate at current time
        ! and when sprinkler first activated.  make sure that specified
        ! heat release rate is the smaller of rate at current time
        ! and rate at sprinkler activation time*exp( ...)
        dtectptr => detectorinfo(id)
        tdrate = dtectptr%tau
        xxtimef = dtectptr%activation_time - fireptr%ignition_time
        call interp(fireptr%t_qdot,fireptr%qdot,fireptr%n_qdot,xxtime,1,qt)
        call interp(fireptr%t_qdot,fireptr%qdot,fireptr%n_qdot,xxtimef,1,qtf)
        tfact = exp(-(xxtime-xxtimef)/tdrate)
        if (qt<tfact*qtf) then
            ! current time HRR is smaller than sprinklerd value
            ! so use current HRR and reset ifact to 0 so future HRRs are not throttled
            ifact = 0
        else
            xxtime = xxtimef
            ifact = 1
        end if
    end if

    call interp(fireptr%t_mdot,fireptr%mdot,fireptr%n_mdot,xxtime,1,mdot_t)
    call interp(fireptr%t_qdot,fireptr%qdot,fireptr%n_qdot,xxtime,1,qdot_t)
    call interp(fireptr%t_hoc,fireptr%hoc,fireptr%n_hoc,xxtime,1,hoc_t)
    call interp(fireptr%t_soot,fireptr%y_soot,fireptr%n_soot,xxtime,1,y_soot)
    if (xxtime>=fireptr%flaming_transition_time) then
        y_soot_flaming = y_soot
        y_soot_smolder = 0.0_eb
    else
        y_soot_flaming = 0.0_eb
        y_soot_smolder = y_soot
    end if
    call interp(fireptr%t_co,fireptr%y_co,fireptr%n_co,xxtime,1,y_co)
    if (fireptr%n_hcn>0) then
        call interp(fireptr%t_hcn,fireptr%y_hcn,fireptr%n_hcn,xxtime,1,y_hcn)
    else
        y_HCN = fireptr%n_N*0.027028_eb/fireptr%molar_mass
    end if
    call interp(fireptr%t_trace,fireptr%y_trace,fireptr%n_trace,xxtime,1,y_trace)
    call interp(fireptr%t_area,fireptr%area,fireptr%n_area,xxtime,1,area_t)
    call interp(fireptr%t_height,fireptr%height,fireptr%n_height,xxtime,1,height_t)

    n_C = fireptr%n_C
    n_H = fireptr%n_H
    n_O = fireptr%n_O
    n_N = fireptr%n_N
    n_Cl = fireptr%n_Cl

    ! attenuate mass and energy release rates if there is an active sprinkler in this room
    if (id/=0.and.ifact==1) then
        mdot_t = mdot_t*tfact
        qdot_t = qdot_t*tfact
    end if

    tfilter_max=1.0_eb
    if (adiabatic_walls.and.tsec<tfilter_max) then
        factor = tsec/tfilter_max
        mdot_t = mdot_t*factor
        qdot_t = qdot_t*factor
    end if

    return
    end subroutine interpolate_pyrolysis

! --------------------------- do_fire -------------------------------------------

    subroutine do_fire (ifire, iroom, pyrolysis_rate, room_height, room_width, room_depth, hoc, &
        y_soot, y_soot_flaming, y_soot_smolder, y_co, y_hcn, y_trace, n_C, n_H, n_O, n_N, n_Cl, molar_mass, species_mass, &
        x_fire_position, y_fire_position, z_fire_position, fire_area, entrainment_rate, plume_flow_rate, hrr, &
        species_mass_rate, hrr_c, hrr_r, hrr_lower, hrr_upper)

    ! do heat release and species from a fire
    
    ! inputs  ifire:            fire number
    !         iroom:            room containing the fire
    !         pyrolysis_rate    pyrolysis rate of the fire (kg/s)
    !         room_height       height of the room (m)
    !         room_width        breadth of the room (m)
    !         room_depth        depth of the room (m)
    !         hoc               current heat of combustion (j/kg)
    !         y_soot            also y_soot_flaming, y_soot_smolder, y_co, y_trace: species yields for soot, CO, and trace species;
    !                               others are calculated from the molecular formula of the fuel 
    !                               (kg species produced/kg fuel pyrolyzed)
    !         n_C               also n_H, n_O, n_N, n_Cl: molecular formula for the fuel; these can be fractional; yields
    !                               of O2, HCl, and HCN are determined from this
    !         molar_mass        molar mass of the fuel (kg/mol)
    !         species_mass      mass of a species in a layer in the room (kg)
    !         x_fire_position   position of the fire in x direction
    !         y_fire_position   position of the fire in y direction
    !         z_fire_position   position of the fire in z direction
    !         fire_area         characteristic object diameter for plume models
    ! outputs  entrainment_rate     plume entrainment rate (kg/s)
    !          plume_flow_rate      plume flow rate into the upper layer (kg/s)
    !          hrr                  actual heat release rate of the fire (w)
    !          species_mass_rate    net change in mass of a species in a layer
    !          hrr_c                net convection into upper layer (w)
    !          hrr_r                net radiation from fire (w)
    !          hrr_lower            heat release in the lower plume (w)
    !          hrr_upper            heat release rate in the upper plume (w)

    integer, intent(in) :: ifire, iroom
    real(eb), intent(in) :: pyrolysis_rate, room_height, room_width, room_depth, hoc, y_soot, y_soot_flaming, y_soot_smolder, &
                            y_co, y_hcn, y_trace
    real(eb), intent(in) :: n_C ,n_H, n_O, n_N, n_Cl
    real(eb), intent(in) :: molar_mass, species_mass(2,ns), x_fire_position, y_fire_position, z_fire_position, fire_area
    real(eb), intent(out) :: entrainment_rate, plume_flow_rate, species_mass_rate(2,ns), hrr_c, hrr_r, hrr_lower, hrr_upper

    real(eb) :: xmass(ns), layer_thickness, t_lower, t_upper, lower_plume_height, upper_plume_height
    real(eb) :: pyrolysis_rate_constrained, hrr_constrained
    real(eb) :: chirad, hrr, source_o2, xtemp, pyrolysis_rate_upper, plume_flow_rate_upper, entrainment_rate_upper
    real(eb) :: firex, firey
    integer :: ipass, lsp
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr

    roomptr => roominfo(iroom)
    fireptr => fireinfo(ifire)

    layer_thickness = roomptr%depth(u)
    t_lower = roomptr%temp(l)
    t_upper = roomptr%temp(u)
    
    hrr = 0.0_eb
    hrr_c = 0.0_eb
    hrr_r = 0.0_eb
    entrainment_rate = 0.0_eb

    pyrolysis_rate_constrained = 0.0_eb
    plume_flow_rate = 0.0_eb

    species_mass_rate(u,1:ns) = 0.0_eb
    species_mass_rate(l,1:ns) = 0.0_eb
    xmass(1:ns) = 0.0_eb

    ! the trace species is assumed to be released by the pyrolysis of the burning object regardless of
    ! whether the fuel actually combusts here. this is consistent with the earlier chemistry routine.
    ! release it here and deposit it in the upper layer
    species_mass_rate(u,ts) = pyrolysis_rate*y_trace
    
    ! fuel released by pyrolysis is put into the upper layer. The fuel that burns in the plume is calculated
    ! in the chemistry routine and then removed from the upper layer
    species_mass_rate(u,fuel) = pyrolysis_rate

    ! divvy up the plume output into radiation and convective energy.
    ! convection drives the plume entrainment

    hrr_lower = 0.0_eb
    hrr = pyrolysis_rate*hoc
    chirad = max(min(fireptr%chirad,1.0_eb),0.0_eb)
    hrr_c = max(hrr*(1.0_eb-chirad),0.0_eb)
    
    ! set wall or corner fire
    firex = min(x_fire_position,room_width-x_fire_position)
    firey = min(y_fire_position,room_depth-y_fire_position)
    fireptr%modified_plume = 1
    if (firex<=sqrt(fireptr%firearea)/2.or.firey<=sqrt(fireptr%firearea)/2) fireptr%modified_plume = 2
    if (firex<=sqrt(fireptr%firearea)/2.and.firey<=sqrt(fireptr%firearea)/2) fireptr%modified_plume = 3

    lower_plume_height = room_height - layer_thickness - z_fire_position
    ! lower layer burning
    if (lower_plume_height>0.0_eb) then
        ! note that the combination of fire_plume and chemistry can be called multiple times
        ! in a single iteration to make sure that the plume entrainment is
        ! consistent with the actual fire size for oxygen limited fires
        ! this is done by "re-passing" the actual fire size to fire_plume in the
        ! next pass. Little seems to be gained by going through loop more than twice
        ipass = 1
        do while (ipass<=5)

            ! calculate the entrainment rate but constrain the actual amount
            ! of air entrained to that required to produce stable stratification
            call heskestad_plume(hrr, hrr_c, lower_plume_height, interior_ambient_temperature, &
                pyrolysis_rate, plume_flow_rate, entrainment_rate, fire_area, &
                min(x_fire_position,room_width-x_fire_position), min(y_fire_position,room_depth-y_fire_position))

            if (roomptr%mass(l)-entrainment_rate <= roomptr%vmin*roomptr%rho(l)) then
                entrainment_rate = max(0.0_eb,roomptr%mass(l)-roomptr%vmin*roomptr%rho(l))
            end if

            entrainment_rate = min(entrainment_rate,hrr_c/(max((t_upper-t_lower),1.0_eb)*cp))
            plume_flow_rate = pyrolysis_rate + entrainment_rate

            ! now do the kinetics scheme
            source_o2 = roomptr%species_fraction(l,o2)
            call chemistry (pyrolysis_rate, molar_mass, entrainment_rate, hoc, y_soot, y_soot_flaming, y_soot_smolder, y_co, &
                y_hcn, n_C, n_H, n_O, n_N, n_Cl, source_o2, lower_o2_limit, hrr_constrained, pyrolysis_rate_constrained, xmass)

            ! next guess for plume entrainment just uses the calculated constrained HRR
            if (abs(hrr-hrr_constrained)>atol) then
                hrr = hrr_constrained
                hrr_c = hrr_constrained*(1._eb-chirad)
                ipass = ipass + 1
                cycle
            end if
            exit
        end do
        plume_flow_rate = pyrolysis_rate + entrainment_rate

        species_mass_rate(u,1:ns) = xmass(1:ns) + species_mass_rate(u,1:ns)

        hrr_lower = hrr_constrained
    end if
    
    ! add burning in the upper layer to the fire. the heat which drives entrainment in the upper layer is the sum of the
    ! heat released in the lower layer and what can be released in the upper layer.
    hrr_upper = 0.0_eb
    pyrolysis_rate_upper = max(0.0_eb,pyrolysis_rate-pyrolysis_rate_constrained)

    if (pyrolysis_rate_upper>0.0_eb) then
        hrr_c = hoc*pyrolysis_rate_upper*(1.0_eb-chirad) + hrr_lower*(1.0_eb-chirad)
        hrr = hrr_c/(1.0_eb-chirad)
        upper_plume_height = max (0.0_eb, min(layer_thickness,(room_height - z_fire_position)))

        call heskestad_plume (hrr, hrr_c, upper_plume_height, interior_ambient_temperature, pyrolysis_rate_upper, &
            plume_flow_rate_upper, entrainment_rate_upper, fire_area, &
           min(x_fire_position,room_width-x_fire_position), min(y_fire_position,room_depth-y_fire_position))

        source_o2 = roomptr%species_fraction(u,o2)
        call chemistry (pyrolysis_rate_upper, molar_mass, entrainment_rate_upper, hoc, y_soot, y_soot_flaming, y_soot_smolder, &
            y_co, y_hcn, n_C, n_H, n_O, n_N, n_Cl, source_o2, lower_o2_limit, hrr_constrained, pyrolysis_rate_constrained, xmass)

        species_mass_rate(u,1:ns) = xmass(1:ns) + species_mass_rate(u,1:ns)
        hrr_upper = hrr_constrained
    end if
    
    hrr = hrr_lower + hrr_upper
    hrr_r = hrr*chirad
    hrr_c = hrr*(1.0_eb-chirad)
    
    ! keep track of unburned fuel composition because it may burn later in a door jet
    if (species_mass_rate(u,fuel)>0.0_eb) then
        pyrolysis_rate_upper = species_mass_rate(u,fuel)
        source_o2 = lower_o2_limit+0.02
        entrainment_rate_upper = 1000.0_eb*(pyrolysis_rate_upper*hoc)/(o2f*source_o2)
        call chemistry (pyrolysis_rate_upper, molar_mass, entrainment_rate_upper, hoc, y_soot, y_soot_flaming, y_soot_smolder, &
            y_co, y_hcn, n_C, n_H, n_O, n_N, n_Cl, source_o2, lower_o2_limit, hrr, pyrolysis_rate_constrained, xmass)
        species_mass_rate(u,fuel_moles) = pyrolysis_rate_upper/molar_mass
        species_mass_rate(u,fuel_Q) = hrr
        species_mass_rate(u,fuel_o2) = -xmass(o2)
        species_mass_rate(u,fuel_co2) = xmass(co2)
        species_mass_rate(u,fuel_co) = xmass(co)
        species_mass_rate(u,fuel_hcn) = xmass(hcn)
        species_mass_rate(u,fuel_hcl) = xmass(hcl)
        species_mass_rate(u,fuel_h2o) = xmass(h2o)
        species_mass_rate(u,fuel_soot) = xmass(soot)
        species_mass_rate(u,fuel_n2) = xmass(n2)
    end if 
    
    ! normalize the species yields to unity
    xtemp = 0.0_eb
    do lsp = 1, ns_mass
        xtemp = xtemp + species_mass(l,lsp)
    end do
    if (xtemp==0.0_eb) xtemp = 1.0_eb
    species_mass_rate(u,1:ns) = species_mass_rate(u,1:ns) + entrainment_rate*species_mass(l,1:ns)/xtemp
    species_mass_rate(l,1:ns) = species_mass_rate(l,1:ns) - entrainment_rate*species_mass(l,1:ns)/xtemp
    
    return
    end subroutine do_fire

! --------------------------- heskestad plume -------------------------------------------

    subroutine heskestad_plume (q_t, q_c, z, t_inf, pyrolysis_rate, plume_flow_rate, entrainment_rate, fire_area, x, y)

    ! calculates plume entrainment for a fire from heskestad's variant of zukoski's correlation
    
    ! inputs    q_t                 fire size (w)
    !           z                   plume height (m)
    !           t_inf               ambient temperature at base of the fire
    !           pyrolysis_rate      mass loss rate of the fire (kg/s)
    !           fire_area           cross sectional area at the base of the fire
    !           x                   distance from fire to wall in x direction (m)
    !           y                   distance from fire to wall in y direction (m)
    ! outputs   plume_flow_rate     total mass transfer rate up to height z (kg/s)
    !           entrainment_rate    net entrainment rate up to height z (kg/s)

    real(eb), intent(in) :: q_t, q_c, z, t_inf, pyrolysis_rate, fire_area, x, y
    real(eb), intent(out) :: plume_flow_rate, entrainment_rate

    real(eb), parameter :: cpg = cp/1000._eb ! correlation uses different units
    real(eb) :: d, qj, z0, z_l, deltaz, xf, factor, qstar, rho_inf
    real(eb) :: c1, c2

    ! determine which entrainment factor to use by fire position.  if we're on the wall or in the corner, entrainment is modified.
    ! by reflection, entrainment on a wall is 1/2 the entrainment of a fire 2 times larger;
    !                            in a corner, 1/4 the entrainment of a fire 4 times larger
    xf = 1.0_eb
    ! V&V experiments show no effect for walls
    if (x<=sqrt(fire_area)/2.or.y<=sqrt(fire_area)/2) xf = 1.0_eb 
    if (x<=sqrt(fire_area)/2.and.y<=sqrt(fire_area)/2) xf = 4.0_eb

    ! qstar and virtual origin correlation are based on total HRR
    qj = 0.001_eb*q_t*xf
    if (z>0.0_eb.and.qj>0.0_eb) then
        d = sqrt(fire_area*xf/pio4)
        rho_inf = 352.981915_eb/t_inf
        qstar = qj/(rho_inf*cpg*t_inf*gsqrt*d**(2.5_eb))
        z0 = d*(-1.02_eb + 1.4_eb*qstar**0.4_eb)

        ! entrainment is based on convective HRR and the mean flame height
        qj = 0.001_eb*q_c*xf
        z_l = max(0.0001_eb,d*(-1.02_eb + 3.7*qstar**0.4_eb))
        if (z>z_l) then
            factor = 1.0_eb
            deltaz = max(0.0001_eb, z-z0)
        else
            factor = z/z_l
            deltaz = max(0.0001_eb, z_l-z0)
        end if
        c1 = 0.196*(grav_con*rho_inf**2/(cpg*t_inf))**onethird  ! under normal conditions, 0.071_eb
        c2 = 2.9_eb/((gsqrt*cpg*rho_inf*t_inf)**twothirds)      ! under normal conditions, 0.026_eb
        entrainment_rate = (c1*qj**onethird*deltaz**(5.0_eb/3.0_eb)*(1.0_eb+c2*qj**twothirds*deltaz**(-5.0_eb/3.0_eb)) * factor)/xf
        plume_flow_rate = pyrolysis_rate + entrainment_rate
    else
        plume_flow_rate = pyrolysis_rate
        entrainment_rate = 0.0_eb
    end if

    end subroutine heskestad_plume

! --------------------------- chemistry -------------------------------------------

    subroutine chemistry (pyrolysis_rate, molar_mass, entrainment_rate, hoc, y_soot, y_soot_flaming, y_soot_smolder, &
       y_co, y_hcn, n_C, n_H, n_O, n_N, n_Cl, source_o2, lower_o2_limit, hrr_constrained, pyrolysis_rate_constrained, species_rates)

    ! do the combustion chemistry - for plumes in both the upper and lower layers.
    ! note that the kinetics scheme is implemented here.  however, applying it to the
    ! various pieces, namely the lower layer plume, the upper layer plume, and the door jet fires, is
    ! somewhat complex. Care should be exercised in making changes either here or in the source interface routine.
    
    !inputs     pyrolysis_rate      calculated pyrolysis rate of the fuel (kg/s)
    !           molar_mass          molar mass of the fuel (kg/mol)
    !           entrainment_rate    calculated entrainment rate (kg/s)
    !           source_room         compartment that contains this fire
    !           hoc                 heat of combustion of the fuel (W/kg)
    !           y_soot              also y_soot_flaming, y_soot_smolder: species yields for soot
    !           y_co                species yield for CO
    !           y_hcn               species yield for HCN, may be limited by the amount of nitrogen in the fuel
    !                               others are calculated from the molecular formula of the fuel (kg species produced/kg fuel pyrolyzed)
    !           n_C                 also n_H, n_O, n_N, n_Cl: molecular formula for the fuel; these can be fractional;
    !                               yields of O2, HCl, and HCN are determined from this
    !           source_o2           oxygen concentration in the source layer of the compartment
    !           lower_o2_limit      lower oxygen limit for combustion (as a fraction)
    ! outputs   hrr_constrained     actual HRR of the fire constrained by available oxygen (W)
    !                               pyrolysis_rate_constrained (output): actual pyrolysis rate of the fuel
    !                               constrained by available oxygen (kg/s)
    !           species_rates       production rates of species based on calculated yields and constrained
    !                               pyrolysis rate (kg/s); fuel and oxygen are naturally negative

    real(eb), intent(in) :: pyrolysis_rate, molar_mass, entrainment_rate, hoc, y_soot, y_co, y_hcn, n_C, n_H, n_O, n_N, n_Cl
    real(eb), intent(in) :: y_soot_flaming, y_soot_smolder
    real(eb), intent(in) :: source_o2, lower_o2_limit
    real(eb), intent(out) :: hrr_constrained, pyrolysis_rate_constrained, species_rates(:)

    real(eb) :: o2_entrained, o2_factor, o2_available
    real(eb) :: nu_o2, nu_co2, nu_h2o, nu_co, nu_soot, nu_hcl, nu_hcn, nu_n2, nu_soot_flaming, nu_soot_smolder
    real(eb) :: net_n2, net_o2, net_co2, net_h2o, net_co, net_soot, net_hcl, net_hcn, net_fuel, net_ct, net_soot_flaming, &
        net_soot_smolder
    real(eb) :: factor

    ! calculate the actual burning rate constrained by available o2.

    ! note the scaling in the tanh function.  tanh approaches ~2 at
    ! about ~4. the function inside the tanh scales the ordinate to
    ! ~o2range.  the remainder of the function scales the result
    ! to 0-1
    o2_entrained = entrainment_rate*source_o2
    o2_factor = tanhsmooth(source_o2, lower_o2_limit+0.01_eb, lower_o2_limit, 1.0_eb, 0.0_eb)
    o2_available = o2_entrained*o2_factor
    hrr_constrained = max(0.0_eb,min(pyrolysis_rate*hoc,o2_available*o2f))
    pyrolysis_rate_constrained = hrr_constrained/hoc

    ! now do the chemistry balance with supplied inputs.
    nu_soot = molar_mass/0.01201_eb*y_soot
    nu_soot_flaming = molar_mass/0.01201_eb*y_soot_flaming
    nu_soot_smolder = molar_mass/0.01201_eb*y_soot_smolder
    nu_hcn = min(n_N,molar_mass/0.027028_eb*y_hcn)
    nu_hcl = n_Cl
    nu_co = molar_mass/0.02801_eb*y_co
    nu_h2o = (n_H - nu_hcl - nu_hcn)/2.0_eb
    nu_co2 = n_C  - nu_co - nu_hcn - nu_soot
    nu_o2 = nu_co2 + (nu_h2o + nu_co - n_O)/2.0_eb
    nu_n2 = (n_N - nu_hcn)/2.0_eb

    ! chemistry balance is molar-based so convert back to mass rates. fuel and o2 are consumed,
    ! so negative. Others are produced, so positive
    net_fuel = -pyrolysis_rate_constrained
    net_o2 = -pyrolysis_rate_constrained*nu_o2*0.032_eb/molar_mass
    net_co2 = pyrolysis_rate_constrained*nu_co2*0.04401_eb/molar_mass
    net_co = pyrolysis_rate_constrained*nu_co*0.02801_eb/molar_mass
    net_h2o = pyrolysis_rate_constrained*nu_h2o*0.018016_eb/molar_mass
    net_hcl = pyrolysis_rate_constrained*nu_hcl*0.036458_eb/molar_mass
    net_hcn = pyrolysis_rate_constrained*nu_hcn*0.027028_eb/molar_mass
    net_soot = pyrolysis_rate_constrained*nu_soot*0.01201_eb/molar_mass
    net_soot_flaming = pyrolysis_rate_constrained*nu_soot_flaming*0.01201_eb/molar_mass
    net_soot_smolder = pyrolysis_rate_constrained*nu_soot_smolder*0.01201_eb/molar_mass
    net_n2 = pyrolysis_rate_constrained*nu_n2*0.02802_eb/molar_mass
    net_ct = 0.0_eb
    
    if (-net_o2 > o2_available) then
        factor = -o2_available/net_o2
        pyrolysis_rate_constrained = factor*pyrolysis_rate_constrained
        hrr_constrained = factor*hrr_constrained
        net_fuel = factor*net_fuel
        net_o2 = factor*net_o2
        net_co2 = factor*net_co2
        net_co = factor*net_co
        net_h2o = factor*net_h2o
        net_hcl = factor*net_hcl
        net_hcn = factor*net_hcn
        net_soot = factor*net_soot
        net_soot_flaming = factor*net_soot_flaming
        net_soot_smolder = factor*net_soot_smolder
        net_n2 = factor*net_n2
    end if 

    ! set mass "generation" rates in the cfast structure for species
    species_rates(n2) = net_n2
    species_rates(o2) = net_o2
    species_rates(co2) = net_co2
    species_rates(co) = net_co
    species_rates(hcn) = net_hcn
    species_rates(hcl) = net_hcl
    species_rates(fuel) = net_fuel
    species_rates(h2o) = net_h2o
    species_rates(soot) = net_soot
    species_rates(soot_flaming) = net_soot_flaming
    species_rates(soot_smolder) = net_soot_smolder
    species_rates(ct) = net_ct

    end subroutine chemistry

! --------------------------- integrate_mass -------------------------------------------

    subroutine integrate_mass (tsec, deltt)

    ! routine to integrate the pyrolysis rate of species to get total mass. We also integrate the trace species release 
    !   and total for all fires
    
    ! inputs    tsec    current simulation time
    !           deltt   current time step

    real(eb), intent(in) :: tsec, deltt

    integer ::i
    real(eb) :: fraction
    character(64) :: rampid
    type(fire_type), pointer :: fireptr
    type(vent_type), pointer :: ventptr

    do i = 1, n_fires
        fireptr => fireinfo(i)
        fireptr%total_pyrolysate = fireptr%total_pyrolysate + fireptr%mdot_pyrolysis*deltt
        fireptr%total_trace = fireptr%total_trace + fireptr%mdot_trace*deltt
    end do

    ! sum the trace release from all of the fires
    summed_total_trace = 0.0_eb
    do i = 1, n_fires
        fireptr => fireinfo(i)
        summed_total_trace = summed_total_trace + fireptr%total_trace
    end do

    ! sum the hvac flow
    ! ...%total_trace_flow is the trace species which gets through the vent, ...%total_trace_filtered is the mass stopped.
    do i = 1, n_mvents
        ventptr => mventinfo(i)
        rampid = ventptr%ramp_id
        call get_vent_opening (rampid,'F',ventptr%room1,ventptr%room2,ventptr%counter,i,tsec,fraction)
        fraction = (1.0_eb-fraction)
        ventptr%total_flow(u) = ventptr%total_flow(u) + ventptr%mflow(1,u)*deltt
        ventptr%total_flow(l) = ventptr%total_flow(l) + ventptr%mflow(1,l)*deltt
        ventptr%total_trace_flow(u) = ventptr%total_trace_flow(u) + ventptr%mflow(1,u)*ventptr%species_fraction(u,ts)*fraction*deltt
        ventptr%total_trace_flow(l) = ventptr%total_trace_flow(l) + ventptr%mflow(1,l)*ventptr%species_fraction(l,ts)*fraction*deltt
        ventptr%total_trace_filtered(u)  = ventptr%total_trace_filtered(u) + &
            ventptr%mflow(1,u)*ventptr%species_fraction(u,ts)*(1.0_eb-fraction)*deltt
        ventptr%total_trace_filtered(l)  = ventptr%total_trace_filtered(l) + &
            ventptr%mflow(1,l)*ventptr%species_fraction(l,ts)*(1.0_eb-fraction)*deltt
    end do

    return
    end subroutine integrate_mass

! --------------------------- door_jets -------------------------------------------

    subroutine door_jets (flows_doorjets, djetflg)

    ! physical interface routine to calculate the current
    ! calculates rates of mass and energy flows into the layers from all door jet fires in the building.
    ! note that we presume that this calculation is performed after the normal fires and flow through vents so we
    ! have an effetive heat of combustion to use for the burning fuel. 

    ! input     nfire           total number of normal fires
    ! output    flows_doorjets  mass and energy flows into layers due to fires.

    logical, intent(out) :: djetflg
    real(eb), intent(out) :: flows_doorjets(mxrooms,ns+2,2)

    real(eb) :: species_mass_rate1(2,ns), species_mass_rate2(2,ns), flw1to2, flw2to1, qpyrol1, qpyrol2
    integer :: i, iroom1, iroom2

    logical :: dj1flag, dj2flag
    type(vent_type), pointer :: ventptr
    type(room_type), pointer :: roomptr, room1ptr, room2ptr

    ! initialize summations and local data

    flows_doorjets(1:nr,1:ns+2,l) = 0.0_eb
    flows_doorjets(1:nr,1:ns+2,u) = 0.0_eb
    roominfo(1:nr)%qdot_doorjet = 0.0_eb
    djetflg = .false.
    if (option(fdfire)/=on.or.n_fires<=0) return

    ! if no vents have a door jet fire then exit
    do i = 1, n_hvents
        ventptr=>hventinfo(i)

        ! is there a door jet fire into room iroom1
        iroom1 = ventptr%room1
        room1ptr => roominfo(iroom1)
        if (room1ptr%temp(u)>=tgignt) then
            flw1to2 = vss(1,i)+vsa(1,i)
            if (vsas(2,i)>0.0_eb.and.flw1to2>0.0_eb) then
                djetflg = .true.
                exit
            end if
        end if

        !is there a door jet fire into room iroom2
        iroom2 = ventptr%room2
        room2ptr => roominfo(iroom2)
        if (room2ptr%temp(u)>=tgignt) then
            flw2to1 = vss(2,i)+vsa(2,i)
            if (vsas(1,i)>0.0_eb.and.flw2to1>0.0_eb) then
                djetflg = .true.
                exit
            end if
        end if
    end do
    if (.not.djetflg)return

    ! calculate the heat for each of the door jet fires

        do i = 1, n_hvents
            ventptr=>hventinfo(i)
                iroom1 = ventptr%room1
                iroom2 = ventptr%room2
                room1ptr => roominfo(iroom1)
                room2ptr => roominfo(iroom2)
                flw1to2 = room1ptr%species_fraction(u,fuel)*(vss(1,i)+vsa(1,i))
                flw2to1 = room2ptr%species_fraction(u,fuel)*(vss(2,i)+vsa(2,i))
                call door_jet_fire (iroom2,iroom1,flw1to2,vsas(2,i),qpyrol2,species_mass_rate2,dj2flag)
                call door_jet_fire (iroom1,iroom2,flw2to1,vsas(1,i),qpyrol1,species_mass_rate1,dj1flag)

                ! sum the flows for return to the source routine
                if (dj1flag) then
                    flows_doorjets(iroom1,q,u) = flows_doorjets(iroom1,q,u) + qpyrol1
                    flows_doorjets(iroom1,3:ns+2,u) = flows_doorjets(iroom1,3:ns+2,u) + species_mass_rate1(u,1:ns)
                end if
                if (dj2flag) then
                    flows_doorjets(iroom2,q,u) = flows_doorjets(iroom2,q,u) + qpyrol2
                    flows_doorjets(iroom2,3:ns+2,u) = flows_doorjets(iroom2,3:ns+2,u) + species_mass_rate2(u,1:ns)
                end if
        end do

    do i = 1, nr
        roomptr => roominfo(i)
        roomptr%qdot_doorjet = flows_doorjets(i,q,u) + flows_doorjets(i,q,l)
    end do
    return
    end subroutine door_jets

! --------------------------- door_jet_fire -------------------------------------------

    subroutine door_jet_fire (ito, ifrom, netfuel, entrainment_rate, hrr, species_mass_rate, djflowflg)

    ! calculate heat and combustion chemistry for a door jet fire
    
    ! inputs    ito                 room number door jet is flowing into
    !           ifrom               room number door jet is flowing from
    !           netfuel             net fuel available to be burned
    !           entrainment_rate    mass flow rate of entrained air in door jet
    ! outputs   hrr                 total heat released by door jet fire
    !           species_mass_rate   net change in mass of each species in door jet
    !           djflowflg           true if there are door jets at this vent

    integer, intent(in) :: ito, ifrom
    real(eb), intent(in) :: netfuel, entrainment_rate
    logical, intent(out) :: djflowflg
    real(eb), intent(out) :: hrr, species_mass_rate(2,ns)

    real(eb) :: xmass(ns), source_o2, xxmolar_mass, hrr_constrained, pyrolysis_rate_constrained, tjet, hoc, flowfrac
    real(eb) :: y_soot, y_flaming_soot, y_smolder_soot, y_co, y_hcn
    real(eb) :: nu_o2, nu_co2, nu_co, nu_hcn, nu_hcl, nu_h2o, nu_soot
    real(eb) :: n_n, n_o, n_c, n_cl,n_h
    type(room_type), pointer :: room1ptr, room2ptr

    hrr = 0.0_eb
    djflowflg = .false.
    room1ptr => roominfo(ito)
    room2ptr => roominfo(ifrom)
    tjet = room2ptr%temp(u)

    ! we only want to do the door jet calculation if there is fuel, oxygen, and sufficient temperature in the door jet
    if (netfuel>0.0_eb.and.entrainment_rate>0.0_eb.and.tjet>=tgignt.and.room2ptr%species_mass(u,fuel_Q)>0.0_eb.and. &
        room2ptr%species_mass(u,fuel_moles)>0.0_eb) then

        djflowflg = .true.
        xmass(1:ns) = 0.0_eb
        
        source_o2 = room1ptr%species_fraction(l,o2)
        hoc = room2ptr%species_mass(u,fuel_Q)/room2ptr%species_mass(u,fuel)
        xxmolar_mass = room2ptr%species_mass(u,fuel)/room2ptr%species_mass(u,fuel_moles)
        y_soot = room2ptr%species_mass(u,fuel_soot)/room2ptr%species_mass(u,fuel)
        y_flaming_soot = y_soot
        y_smolder_soot = 0.0_eb
        y_co = room2ptr%species_mass(u,fuel_co)/room2ptr%species_mass(u,fuel)
        y_hcn = room2ptr%species_mass(u,fuel_hcn)/room2ptr%species_mass(u,fuel)
        nu_soot = y_soot*xxmolar_mass/0.01201_eb
        nu_co = y_co*xxmolar_mass/0.02801_eb
        nu_hcn = y_hcn*xxmolar_mass/0.027028_eb
        nu_hcl = room2ptr%species_mass(u,fuel_hcl)/room2ptr%species_mass(u,fuel)*xxmolar_mass/0.036458_eb
        nu_co2 = room2ptr%species_mass(u,fuel_co2)/room2ptr%species_mass(u,fuel)*xxmolar_mass/0.04401_eb
        nu_h2o = room2ptr%species_mass(u,fuel_h2o)/room2ptr%species_mass(u,fuel)*xxmolar_mass/0.018016_eb
        nu_o2 = room2ptr%species_mass(u,fuel_o2)/room2ptr%species_mass(u,fuel)*xxmolar_mass/0.032_eb
        
        n_n = nu_hcn
        n_cl = nu_hcl
        n_h = 2.0_eb*nu_h2o + nu_hcn + nu_hcl
        n_c = nu_co2 + nu_hcn + nu_co + nu_soot
        n_o = max(0.0_eb,2.0_eb*nu_co2 - 2.0_eb*nu_o2 + nu_h2o + nu_co)
        
        call chemistry (netfuel, xxmolar_mass, entrainment_rate, hoc, y_soot, y_flaming_soot, y_smolder_soot, y_co, &
            y_hcn, n_c, n_h, n_o, n_n, n_cl, source_o2, lower_o2_limit, hrr_constrained, pyrolysis_rate_constrained, xmass)
        hrr = hrr_constrained
        
        species_mass_rate(u,1:ns) = xmass(1:ns)
        species_mass_rate(l,1:ns) = 0.0_eb
        flowfrac = pyrolysis_rate_constrained/room2ptr%species_mass(u,fuel)
        species_mass_rate(u,fuel_moles) = -flowfrac*room2ptr%species_mass(u,fuel_moles)
        species_mass_rate(u,fuel_Q) = -flowfrac*room2ptr%species_mass(u,fuel_Q)
        species_mass_rate(u,fuel_o2) = -flowfrac*room2ptr%species_mass(u,fuel_o2)
        species_mass_rate(u,fuel_co2) = -flowfrac*room2ptr%species_mass(u,fuel_co2)
        species_mass_rate(u,fuel_co) = -flowfrac*room2ptr%species_mass(u,fuel_co)
        species_mass_rate(u,fuel_hcn) = -flowfrac*room2ptr%species_mass(u,fuel_hcn)
        species_mass_rate(u,fuel_hcl) = -flowfrac*room2ptr%species_mass(u,fuel_hcl)
        species_mass_rate(u,fuel_h2o) = -flowfrac*room2ptr%species_mass(u,fuel_h2o)
    end if
    return
    end subroutine door_jet_fire

! --------------------------- flame_height -------------------------------------------

    real(kind=eb) function flame_height (hrr, fire_area)

    ! calculates flame height for a given fire size and fire_area
    ! SFPE handbook, Section 2, Chapter 1
    
    ! inputs    hrr             fire Size (W)
    !           fire_area       area of the base of the fire (m^2)
    ! output    f_height    calculated flame height (m)

    real(eb), intent(in) :: hrr, fire_area
    real(eb)  :: f_height

    real(eb) :: d

    if (fire_area<=0._eb) then
        d = pio4*0.2_eb**2
    else
        d = sqrt(4.0_eb*fire_area/pi)
    end if
    f_height = -1.02_eb*d + 0.235_eb*(hrr/1.0e3_eb)**0.4_eb
    f_height = max (0.0_eb, f_height)
    flame_height = f_height
    return
    end function flame_height


! --------------------------- get_gas_tempandvelocity -------------------------------------------

    subroutine get_gas_tempandvelocity (iroom, x, y, z, tg, vg)

    ! calculate gas temperature nearby a target
    
    ! inputs    iroom   compartment number
    !           x, y, z position of target in compartment
    ! outputs   tg      calculated gas temperature
    !           vg      calculated gas velocity

    integer, intent(in) :: iroom
    real(eb), intent(in) :: x, y, z

    real(eb), intent(out) :: tg, vg(4)

    real(eb) :: hrr, chirad, area, tu, tl, zfire, zlayer, zceil, r, tplume, vplume, tplume_ceiling, vplume_ceiling, tcj, vcj
    real(eb) :: xdistance, ydistance, distance, hall_width, xf, x_fire_position, y_fire_position
    integer :: i
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr

    roomptr => roominfo(iroom)

    ! default is the appropriate layer temperature and a velocity of 0.1 m/s
    if (z>=roomptr%depth(l)) then
        tg = roomptr%temp(u)
    else
        tg = roomptr%temp(l)
    end if
    vg = 0.0_eb
    ! if there is a fire in the room, calculate plume temperature
    do i = 1,n_fires
        fireptr => fireinfo(i)
        if (fireptr%room==iroom) then
            ! determine which entrainment factor to use by fire position.  if we're on the wall or in the corner, entrainment is modified.
            ! by reflection, entrainment on a wall is 1/2 the entrainment of a fire 2 times larger;
            !                            in a corner, 1/4 the entrainment of a fire 4 times larger

            x_fire_position = min(fireptr%x_position, roomptr%cwidth-fireptr%x_position)
            y_fire_position = min(fireptr%y_position, roomptr%cdepth-fireptr%y_position)
            xf = 1.0_eb
            ! V&V experiments show no effect for walls
            if (x_fire_position<=sqrt(fireptr%firearea)/2.or.y_fire_position<=sqrt(fireptr%firearea)/2) xf = 1.0_eb
            if (x_fire_position<=sqrt(fireptr%firearea)/2.and.y_fire_position<=sqrt(fireptr%firearea)/2) xf = 4.0_eb
            hrr = xf*fireptr%qdot_actual
            chirad = fireptr%chirad
            area = xf*fireptr%firearea
            tu = roomptr%temp(u)
            tl = roomptr%temp(l)
            zfire = fireptr%z_position + fireptr%z_offset
            xdistance = x - fireptr%x_position
            if (abs(xdistance)<=mx_hsep) xdistance = 0.0_eb
            ydistance = y - fireptr%y_position
            if (abs(ydistance)<=mx_hsep) ydistance = 0.0_eb
            zlayer = roomptr%depth(l)
            zceil = roomptr%cheight
            r = sqrt(xdistance**2 + ydistance**2)
            if (roomptr%hall) then
                if (roomptr%cdepth>roomptr%cwidth) then
                    distance = ydistance
                    hall_width = roomptr%cwidth
                else
                    distance = xdistance
                    hall_width = roomptr%cdepth
                end if
            else
                hall_width = 0.0_eb
            end if
            ! first calculate plume temperature at desired location
            call get_plume_tempandvelocity (hrr, chirad, area, tu, tl, zfire, zlayer, z, r, tplume, vplume)
            ! include ceiling jet effects if desired location is in the ceiling jet
            call get_plume_tempandvelocity (hrr, chirad, area, tu, tl, zfire, zlayer, zceil, 0.0_eb, &
                tplume_ceiling, vplume_ceiling)
            call get_ceilingjet_tempandvelocity(hrr, tu, tl, tplume_ceiling, zfire, zlayer, zceil, z, distance, r, &
                hall_width, tcj, vcj)
            tg = max(tg,tplume,tcj)
            if (r/=0.0_eb) then
                vg(1) = vg(1) + vcj*xdistance/r
                vg(2) = vg(2) + vcj*ydistance/r
            end if
            vg(3) = vg(3) + vplume
        end if
    end do
    vg(4) = sqrt(vg(1)**2+vg(2)**2+vg(3)**2)
    return

    end subroutine get_gas_tempandvelocity

! --------------------------- get_ceilingjet_tempandvelocity --------------------------------------

    subroutine get_ceilingjet_tempandvelocity (hrr, tu, tl, tplume, zfire, zlayer, zceil, zin, xin, r, w, tcj, vcj)

    ! calculates ceiling jet temperature and velocity at a specified height and distance from the fire.
    !
    ! Uses Alpert / Heskestad's correlation to calculate plume  temperature
    
    ! inputs    hrr    total heat release rate of the fire (W)
    !           tu      upper layer gas temperature (K)
    !           tl      lower layer gas temperature (K)
    !           tplume  plume temperature at ceiling (K)
    !           zfire   height of the base of the fire (m)
    !           zlayer  height of the hot/cold gas layer interface (m)
    !           zceil   height of the compartment ceiling (m)
    !           zin     position to calculate temperature (m)
    !           xin     distance to calculate temperature (parallel to long wall ... for hallways) (m)
    !           r       horizontal distance from fire centerline (m)
    !           w       width of hallway is compartment is designated as a hallway, zero otherwisw
    ! outputs   tcj     temperature at height zin and radius r (K)
    !           vcj     velocity at height zin and radius r (m/s)

    real(eb), intent(in) :: hrr, tu, tl, tplume, zfire, zlayer, zceil, zin, xin, r, w
    real(eb), intent(out) :: tcj, vcj

    real(eb), parameter :: cpg = cp/1000._eb ! correlation uses different units
    real(eb), parameter :: deltaT_0star_at_p2 = (0.225_eb+0.27_eb*0.2_eb)**(-4._eb/3._eb)
    real(eb) :: t_inf, t_layer, rho_inf, qstar_h, h, delta_cj

    ! Set default values
    if (zin<=zlayer) then
        ! desired point is in the lower layer
        t_layer = tl
    else
        t_layer = tu
    end if
    tcj = t_layer
    vcj = 0.0

    h = zceil - zfire

    ! for the temperature algorithm to work, there has to be a fire, two layers, and a target point above the fire
    if (hrr>0.0_eb.and.tu>=tl.and.h>=0.0_eb) then
        if (zfire<=zlayer) then
            ! fire is in the lower layer
            t_inf = tl
        else
            t_inf = tu
        end if
        rho_inf = 352.981915_eb/t_inf

        !ceiling jet thickness
        if (r/h>=0.26_eb) then
            delta_cj = h * 0.112_eb*(1.0_eb-exp(-2.24_eb*r/h))
        else
            delta_cj = h * 0.112_eb*(1.0_eb-exp(-2.24_eb*0.26_eb))
        end if

        if (zin>=zceil-delta_cj) then
            ! if compartment is not a hallway or distance is smaller than 1/2 hallway width, use alpert and heskestad

            !ceiling jet temperature
            if (r/h<=0.2_eb) then
                tcj = tplume
            else
                tcj = t_layer + (tplume-t_layer)/deltaT_0star_at_p2 * (0.225_eb+0.27_eb*r/h)**(-4.0_eb/3.0_eb)
            end if
            if (w>0.0_eb.and.xin>w/2) then
                !compartment is a hallway and we've hit the walls, use delichastsios
                tcj = max(tcj,t_layer + (tplume-t_layer)*0.37_eb*(h/w)**onethird*exp(-0.16_eb*xin/h*(w/h)**onethird))
            end if

            ! ceiling jet velocity
            qstar_h = (hrr/1000._eb)/(rho_inf*cpg*t_inf*gsqrt*h**2.5_eb)
            if (r/h<=0.17_eb) then
                vcj = gsqrt*sqrt(h)*qstar_h**onethird*3.61_eb
            else
                vcj = gsqrt*sqrt(h)*qstar_h**onethird*1.06_eb*(r/h)**(-0.69_eb)
            end if
            if (w>0.0_eb.and.xin>w/2) then
                !compartment is a hallway and we've hit the walls, use delichastsios
                vcj = max(vcj,0.114_eb*sqrt(h*(tcj-t_layer))*(h/w)**(1.0_eb/6.0_eb))
            end if
        end if
    end if

    return
    end subroutine get_ceilingjet_tempandvelocity

! --------------------------- get_plume_tempandvelocity -------------------------------------------

    subroutine get_plume_tempandvelocity (hrr, chirad, area, tu, tl, zfire, zlayer, zin, r, tplume, uplume)

    ! calculates plume centerline temperature at a specified height and distance from the fire.
    !
    ! Uses Heskestad's correlation to calculate plume  temperature with Evan's method to determine virtual 
    ! fire size and fire origin when fire is in the lower layer and position is in the upper layer
    
    ! inputs    hrr         total heat release rate of the fire (W)
    !           chirad        fraction of fire HRR released as radiation
    !           area        fire diamater (m)
    !           tu          upper layer gas temperature (K)
    !           tl          lower layer gas temperature (K)
    !           zfire       height of the base of the fire (m)
    !           zlayer      height of the hot/cold gas layer interface (m)
    !           zin         position to calculate plume centerline temperature (m)
    !           r           horizontal distance from fire centerline (m)
    ! outputs   tplume      plume temperature at height zin and radius r (K)
    !           uplume      plume velocity at height zin and radius r (m/s)

    real(eb), intent(in) :: hrr, chirad, area, tu, tl, zfire, zlayer, zin, r
    real(eb), intent(out) :: tplume, uplume

    real(eb), parameter :: cpg = cp/1000._eb ! correlation uses different units
    real(eb) :: t_inf, rho, hrr_c, qstar, z0, z0_prime, z_flame, deltaz, d, t_excess, sigma_deltat, sigma_u, u_max

    ! default is for temperature to be the layer temperature at the desired location
    if (zin<=zlayer) then
        tplume = tl
    else
        tplume = tu
    end if
    uplume = 0.0_eb

    ! for the algorithm to work, there has to be a fire, two layers, and a target point above the fire
    if (hrr>0.0_eb.and.tu>=tl.and.zin-zfire>=0.0_eb) then
        hrr_c = hrr*(1.0_eb - chirad)/1000.0_eb
        d = sqrt(area/pio4)

        if (zfire<=zlayer) then
            t_inf = tl
        else
            t_inf = tu
        end if

        rho = 352.981915_eb/t_inf
        qstar = (hrr/1000._eb)/(rho*cpg*t_inf*gsqrt*d**2.5_eb)
        z0 = d*(-1.02_eb+1.4_eb*qstar**0.4_eb)
        z_flame = max(0.1_eb,d*(-1.02_eb+3.7_eb*qstar**0.4_eb))

        ! plume temperature
        if (zfire<=zlayer.and.zin>zlayer) then
            ! fire is in lower and and target point is in upper layer
            z0_prime = zlayer-(tu/tl)**0.6_eb * (zlayer-z0)
            rho = 352.981915_eb/tu
            deltaz = max(0.1_eb,zin-zfire-z0_prime)
            t_excess = min(t_max,9.1_eb*(tu/(grav_con*cpg**2*rho**2))**onethird * hrr_c**twothirds * deltaz**(-5.0_eb/3.0_eb))
        else
            ! fire and target point are both in the same layer
            deltaz = max(0.1_eb,zin-zfire-z0)
            t_excess = min(t_max,9.1_eb*(t_inf/(grav_con*cpg**2*rho**2))**onethird * hrr_c**twothirds * deltaz**(-5.0_eb/3.0_eb))
        end if

        ! plume velocity
        u_max = 2.2_eb*(grav_con**0.4_eb/(t_inf**0.4_eb*(cpg*rho)**0.2_eb))*(650._eb*hrr_c)**0.2_eb
        uplume = min(u_max,3.4_eb*(grav_con/(cpg*rho*t_inf))**onethird * hrr_c**onethird * deltaz**(-onethird))

        ! if it's within the flame (assumed to be a cone of diameter d and height equal to flame height, it's flame temperature

        sigma_deltat = 0.14_eb * sqrt(1.0_eb + t_excess/t_inf) * deltaz
        if (r>d*(1.0_eb-(zin-zfire)/z_flame)/2.0_eb) then
            t_excess = t_excess*exp(-(r/sigma_deltat)**2)
        end if
        tplume = t_inf + t_excess

        if (r>0.0_eb) then
            sigma_u = 1.1_eb*sigma_deltat
            uplume = uplume*exp(-(r/sigma_u)**2)
        end if
    end if
    return
    end subroutine get_plume_tempandvelocity

! --------------------------- update_species (toxict) -------------------------------------------

    subroutine update_species (deltt)

    ! calculate species concentrations (molar/volume fraction), mass density (kg/m^3), opacity (1/m), ct (mg-s/m^3)
    
    ! input     deltt   length of the latest time step (s)

    real(eb), intent(in) :: deltt

    ! molar_masses of the species
    real(eb), parameter :: molar_mass(ns_mass) = &
        (/0.02802_eb,0.032_eb,0.04401_eb,0.02801_eb,0.027028_eb,0.036458_eb,0.01201_eb,0.018016_eb,0.01201_eb/)

    ! reciprocal of avagadro's number (so you can't have less than an atom of a species)
    real(eb), parameter :: avagad = 1.0_eb/6.022e23_eb
    
    real(eb) :: air_moles(2), v(2)
    integer i, layer, lsp
    type(room_type), pointer :: roomptr

    do i = 1, nrm1
        roomptr => roominfo(i)
        v(u) = roomptr%volume(u)
        v(l) = roomptr%volume(l)
        do layer = u, l
            air_moles(layer) = 0.0_eb
            do lsp = 1, ns_mass
                if (lsp/=fuel) then
                    air_moles(layer) = air_moles(layer) + roomptr%species_mass(layer,lsp)/molar_mass(lsp)
                else
                    air_moles(layer) = air_moles(layer) + roomptr%species_mass(layer,fuel_moles)
                end if
            end do
            air_moles(layer) = max(avagad,air_moles(layer))
        end do

        ! calculate the mass density in kg/m^3
        do layer = u, l
            roomptr%species_rho(layer,1:ns) = roomptr%species_mass(layer,1:ns)/v(layer)
        end do


        ! calculate the molar fraction (actually in percent)
        do lsp = 1, 8
            do layer = u, l
                if (lsp/=fuel) then
                    roomptr%species_output(layer,lsp) = 100.0_eb*(roomptr%species_mass(layer,lsp)/molar_mass(lsp))/air_moles(layer)
                else
                    roomptr%species_output(layer,fuel) = 100.0_eb*roomptr%species_mass(layer,fuel_moles)/air_moles(layer)
                end if
            end do
        end do

        ! opacity is calculated from seder's work
        ! note: the default value was changed 2/15/2 from 3500 to 3778 to reflect the new value as reported by
        ! mulholland in fire and materials, 24, 227(2000) with recommended value of extinction coefficient
        ! of 8700 m^2/kg or 8700/ln(10)=3778 converted to optical density
        do layer = u, l
            roomptr%species_output(layer,soot_flaming) = roomptr%species_rho(layer,soot_flaming) * sigma_s(flaming)/log(10._eb)
            roomptr%species_output(layer,soot_smolder) = roomptr%species_rho(layer,soot_smolder) * sigma_s(smoldering)/log(10._eb)
            roomptr%species_output(layer,soot) = roomptr%species_output(layer,soot_flaming) + &
                roomptr%species_output(layer,soot_smolder)
        end do

        ! ct is the integration of the total "junk" being transported
        do layer = u, l
            roomptr%species_output(layer,ct) = roomptr%species_output(layer,ct) + &
                roomptr%species_rho(layer,ct)*1000.0_eb*deltt/60.0_eb
        end do

        ! ts (trace species) is the filtered concentration - this is the total mass.
        ! it is converted to fraction of the total generated by all fires.
        ! this step being correct depends on the integratemass routine
        do layer = u, l
            roomptr%species_output(layer,ts) = roomptr%species_mass(layer,ts) !/(summed_total_trace+1.0d-10)
        end do
        
        do lsp = fuel_moles, fuel_soot
            do layer = u, l
                roomptr%species_output(layer,lsp) = roomptr%species_mass(layer,lsp)
            end do
        end do

    end do

    return
    end subroutine update_species

! --------------------------- collect_fire_data_for_smokeview -------------------------------------------

    subroutine collect_fire_data_for_smokeview (nfires)

    ! collect fire information into a single list for smokeview
    
    ! output    nfires  number of fires

    integer, intent(out) :: nfires

    real(eb) :: f_height
    integer :: i
    type(fire_type), pointer :: fireptr

    nfires = n_fires

    do i = 1, n_fires
        fireptr => fireinfo(i)
        smv_xfire(i) = fireptr%x_position
        smv_yfire(i) = fireptr%y_position
        smv_zfire(i) = fireptr%z_position + fireptr%z_offset
        f_height = flame_height (fireptr%qdot_actual, fireptr%firearea)
        smv_qdot(i) = fireptr%qdot_actual
        smv_height(i) = f_height
        smv_room(i) = fireptr%room
    end do
    return
    end subroutine collect_fire_data_for_smokeview

! --------------------------- update_fire_objects -------------------------------------------

    subroutine update_fire_objects (iflag, told, dt, ifobj, tobj)

    ! check for and set object fire ignition
    ! inputs    iflag   flags if check, set, or update variables
    !           told    time previous to this time step
    !           dt      length of last time step
    !           ifobj   object number that ignites (return)
    !           tobj    time object ignites

    integer, intent(in) :: iflag
    integer, intent(out) :: ifobj
    real(eb), intent(in) :: told, dt
    real(eb), intent(out) :: tobj

    real(eb) :: tmpob(2,mxfires), tnobj

    integer :: i, ignflg, itarg

    type(fire_type), pointer :: fireptr
    type(target_type), pointer :: targptr

    ifobj = 0
    tobj = told + 2.0_eb*dt
    tnobj = told + dt

    ! here we just check for ignition of a fire
    do i = 1, n_fires
        fireptr => fireinfo(i)
        if (.not.fireptr%ignited) then
            ignflg = fireptr%ignition_type
            itarg = fireptr%ignition_target
            if (ignflg==trigger_by_time) then
                if (fireptr%ignition_time<=tnobj) then
                    tobj = min(fireptr%ignition_time,tobj)
                    ifobj = i
                    tmpob(1,i) = 1.0_eb
                    tmpob(2,i) = fireptr%ignition_time
                else
                    tmpob(1,i) = 0.0_eb
                    tmpob(2,i) = tnobj + dt
                end if
            else if (ignflg==trigger_by_temp) then
                targptr => targetinfo(itarg)
                call check_object_ignition (told,dt,targptr%temperature(idx_tempf_trg),fireptr%ignition_criterion, &
                    fireptr%temperature,i,ifobj,tobj,tmpob(1,i))
            else if (ignflg==trigger_by_flux) then
                targptr => targetinfo(itarg)
                call check_object_ignition (told,dt,targptr%flux_incident_front,fireptr%ignition_criterion, &
                    fireptr%incident_flux,i,ifobj,tobj,tmpob(1,i))
            else
                call xerror('Update_fire_objects-incorrectly defined ignition type in input file',0,1,1)
                stop
            end if
        end if
    end do

    ! here we process the ignition(s) and set properties if ignited
    if (iflag/=check_state) then
        do i = 1, n_fires
            fireptr => fireinfo(i)
            if (.not.fireptr%ignited) then
                ignflg = fireptr%ignition_type
                itarg = fireptr%ignition_target
                if (ignflg/=trigger_by_time) then
                    targptr => targetinfo(itarg)
                    fireptr%temperature = targptr%temperature(idx_tempf_trg)
                    fireptr%incident_flux = targptr%flux_incident_front
                end if
                if (iflag==set_state.and.tmpob(1,i)>0.0_eb) then
                    if (tmpob(2,i)<=tobj) then
                        fireptr%ignited = .true.
                        fireptr%ignition_time = tmpob(2,i)
                        if (.not.fireptr%reported) then
                            write (iofilo,'(/,a,i0,3a,i0,a)') 'Object #',i,' (',trim(fireptr%name),') ignited at ', &
                                int(max(fireptr%ignition_time+0.5_eb,0.0_eb)),' seconds'
                            write (iofill,'(a,i0,3a,i0,a)') 'Object #',i,' (',trim(fireptr%name),') ignited at ', &
                                int(max(fireptr%ignition_time+0.5_eb,0.0_eb)),' seconds'
                            fireptr%reported = .true.
                        end if
                    end if
                end if
            end if
        end do
    end if

    return
    end subroutine update_fire_objects

! --------------------------- check_object_ignition -------------------------------------------

    subroutine check_object_ignition(told, dt, cond, trip, oldcond, iobj, ifobj, tobj, tmpob)
    
    ! determine if a fire has ignited
    
    ! inputs    iobj    pointer to current fire being tested
    !           told    simulation time at previous time step
    !           dt      current time step
    !           cond    current value of condition being tested (could be time, temperature or heat flux)
    !           trip    trigger value for ignition in units consistent with cond
    !           oldcond previous value of condition being tested
    ! outputs   ifobj   fire number of first item ignited
    !           tobj    ignition time of first item ignited
    !           tmpob   (1) = 1 if ignition has occured for this fire
    !                   (2) = ignition time for this fire

    integer, intent(in) :: iobj
    real(eb), intent(in) :: told, dt, cond, trip, oldcond

    integer, intent(out) :: ifobj
    real(eb), intent(out) :: tmpob(2)
    real(eb), intent(inout) :: tobj

    real(eb) :: delta

    if (cond>trip) then
        delta = (trip-oldcond)/(cond-oldcond)
        tmpob(1) = 1.0_eb
        tmpob(2) = told + dt*delta
        tobj = min(tobj,tmpob(2))
        ifobj = iobj
    else
        tmpob(1) = 0.0_eb
        tmpob(2) = told + 2.0_eb*dt
    end if

    return

    end subroutine check_object_ignition

end module fire_routines
