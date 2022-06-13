module initialization_routines

    use precision_parameters

    use exit_routines, only: cfastexit
    use numerics_routines, only: dnrm2, dscal
    use solve_routines, only : update_data
    use utility_routines, only: indexi    

    use cfast_types, only: detector_type, fire_type, room_type, target_type, material_type, vent_type

    use cenviro, only: constvar, odevara
    use cparams, only: u, l, mxrooms, mxthrmplen, mxmatl, mxhvents, mxvvents, mxmvents, mxleaks, &
        mxdtect, mxtarg, mxslb, mx_vsep, mxtabls, mxfires, pde, interior, nwal, idx_tempf_trg, idx_tempb_trg, &
        xlrg, default_grid, face_front, trigger_by_time, h2o, ns_mass, w_from_room, w_to_room, w_from_wall, w_to_wall, &
        smoked, mx_dumps, mxss
    use defaults, only: default_temperature, default_pressure, default_relative_humidity, default_rti, &
        default_activation_temperature, default_lower_oxygen_limit, default_radiative_fraction
    use devc_data, only: n_detectors, detectorinfo, n_targets, targetinfo, alloc_devc, init_devc
    use dump_data, only: n_dumps, dumpinfo, csvnames, iocsv_compartments, iocsv_vents, iocsv_masses, iocsv_walls, iocsv_devices, &
        alloc_dump, init_dump
    use fire_data, only: n_fires, fireinfo, n_tabls, tablinfo, n_furn, mxpts, lower_o2_limit, tgignt, summed_total_trace, &
        alloc_fire, init_fire
    use material_data, only: n_matl, material_info, alloc_matl, init_matl
    use option_data, only: foxygen, option, on
    use room_data, only: n_rooms, ns, roominfo, initial_mass_fraction, exterior_abs_pressure, interior_abs_pressure, &
        exterior_ambient_temperature, interior_ambient_temperature, exterior_rho, interior_rho, pressure_ref, &
        pressure_offset, relative_humidity, adiabatic_walls, t_ref, n_vcons, vertical_connections, n_cons, nnodes, nwpts, &
        slab_splits, alloc_room, init_room
    use setup_data, only: iofill, debugging, deltat, init_scalars, errormessage
    use solver_data, only: p, maxteq, stpmin, stpmin_cnt, stpmin_cnt_max, stpminflag, nofp, nofwt, noftu, nofvu, noftl, &
        nofoxyu, nofoxyl, nofprd, nequals, i_speciesmap, jaccol
    use spreadsheet_output_data, only: n_sscomp, sscompinfo, n_ssdevice, ssdeviceinfo, n_sswall, sswallinfo, &
        n_ssmass, ssmassinfo, n_ssvent, ssventinfo, alloc_ss, init_ss
    use vent_data, only: n_hvents, hventinfo, n_vvents, vventinfo, n_mvents, mventinfo, n_leaks, leakinfo, alloc_vent, init_vent

    implicit none

    private

    public get_thermal_property, initialize_leakage, initialize_targets, initialize_ambient, initialize_solver_vector, &
        initialize_memory, initialize_fires, initialize_species, initialize_walls

    contains


! --------------------------- get_thermal_property -------------------------------------------

!> \brief   check for and return index to a thermal property
    
!> \param   name (input): desired thermal property name
!> \param   tp (output): thermal property number of desired thermal property

    subroutine get_thermal_property (name, tp)
    
    implicit none
    character(len=*), intent(in) :: name
    integer, intent(out) :: tp

    character(len=mxthrmplen) missingtpp
    integer i
    type(material_type), pointer :: thrmpptr

    do i = 1, n_matl
        thrmpptr => material_info(i)
        if (name==thrmpptr%id) then
            tp = i
            return
        end if
    end do
    missingtpp = name
    write (errormessage,'(''***Error, A thermal property was not found in the input file. Missing material: '',a)') missingtpp
    call cfastexit('get_thermal_property',1)
    stop

    end subroutine get_thermal_property

! --------------------------- initialize_ambient -------------------------------------------

!> \brief   compute initializations for variables related to ambient conditions.  
!>          this initialization is done after we read in the input file

    subroutine initialize_ambient ()

    real(eb) :: dummy(1) = (/0.0_eb/), xxpmin, tdspray, tdrate, scale
    integer i, ii, iwall, iroom, itarg

    type(target_type), pointer :: targptr
    type(room_type), pointer :: roomptr
    type(detector_type), pointer :: dtectptr

    ! make initial pressure calculations consistent
    do i = 1, n_rooms
        roomptr => roominfo(i)
        roomptr%interior_relp_initial = -interior_rho*grav_con*roomptr%z0
        roomptr%exterior_relp_initial = -exterior_rho*grav_con*roomptr%z0
    end do
    roomptr => roominfo(n_rooms+1)
    roomptr%exterior_relp_initial = 0.0_eb


    ! normalize pressures so that the smallest pressure is zero
    roomptr => roominfo(1)
    xxpmin = min(roomptr%interior_relp_initial,roomptr%exterior_relp_initial)
    do i = 2, n_rooms
        roomptr => roominfo(i)
        xxpmin = min(xxpmin,roomptr%interior_relp_initial,roomptr%exterior_relp_initial)
    end do
    do i = 1, n_rooms
        roomptr => roominfo(i)
        roomptr%interior_relp_initial = roomptr%interior_relp_initial - xxpmin
        roomptr%exterior_relp_initial = roomptr%exterior_relp_initial - xxpmin
    end do
    pressure_offset = pressure_offset + xxpmin
    interior_abs_pressure = interior_abs_pressure - pressure_offset
    exterior_abs_pressure = exterior_abs_pressure - pressure_offset

    ! copy all of the variables from the initial values into the data arrays
    call update_data (dummy,constvar)

    ! define the p array, the solution to the ode
    do i = 1, n_rooms
        roomptr => roominfo(i)
        p(i) = roomptr%interior_relp_initial
        p(i+noftu) = interior_ambient_temperature
        p(i+nofvu) = roomptr%vmin
        if (roomptr%shaft) p(i+nofvu) = roomptr%vmax
        p(i+noftl) = interior_ambient_temperature
    end do

    ! define interior surface wall temperatures
    ii = nofwt
    do i = 1, n_rooms
        roomptr => roominfo(i)
        do iwall = 1, nwal
            if (roomptr%surface_on(iwall)) then
                ii = ii + 1
                p(ii) = interior_ambient_temperature
            end if
        end do
    end do

    ! establish default values for detector data
    do i = 1, n_detectors
        dtectptr => detectorinfo(i)
        iroom=dtectptr%room
        roomptr => roominfo(iroom)
        if (dtectptr%center(1)<0.0_eb) dtectptr%center(1) = roomptr%cwidth*0.5_eb
        if (dtectptr%center(2)<0.0_eb) dtectptr%center(2) = roomptr%cdepth*0.5_eb
        if (dtectptr%center(3)<0.0_eb) dtectptr%center(3) = roomptr%cheight-mx_vsep

        ! if tdspray>0 then interpret it as a spray density and convert
        ! to a characteristic quenching time
        ! if tdspray < 0 then interpret abs(tdspray) as the time
        ! required to reduce the fire size by 50 per cent
        ! if tdspray = 0 then turn the sprinkler off
        tdspray = dtectptr%spray_density
        if (tdspray>0.0_eb) then
            tdrate = 3.0_eb/tdspray**1.8_eb
        else if (tdspray<0.0_eb) then
            tdrate = abs(tdspray)/log(2.0_eb)
            tdspray = (3.0_eb/tdrate)**(1.0_eb/1.8_eb)
        else
            tdspray = 0.0_eb
            tdrate = 1.0e10_eb
            dtectptr%quench = .false.
        end if
        dtectptr%spray_density = tdspray
        dtectptr%half_life = tdrate*log(2.0_eb)
        dtectptr%tau = tdrate

        ! set initial ceiling jet and detector link temperatures to ambient
        if (dtectptr%dtype==smoked) then
            dtectptr%value = 0.0_eb
            dtectptr%value_o = 0.0_eb
        else
            dtectptr%value = interior_ambient_temperature
            dtectptr%value_o = interior_ambient_temperature
        end if
        dtectptr%temp_gas = interior_ambient_temperature
        dtectptr%temp_gas_o = interior_ambient_temperature
    end do

    ! p's for pressure, volume and temperature are defined
    ! we can now copy these values to the environment variables
    call update_data (p, odevara)

    ! initialize target temperatures
    do itarg = 1, n_targets
        targptr => targetinfo(itarg)
        iroom = targptr%room
        targptr%temperature(idx_tempf_trg:idx_tempb_trg) = interior_ambient_temperature
        targptr%tgas = interior_ambient_temperature

        ! scale normal vectors to have length 1
        scale = 1.0_eb/dnrm2(3,targptr%normal,1)
        call dscal(3,scale,targptr%normal,1)
    end do

    ! initialize solver oxygen values if required.   (must be initialized after layer mass is defined)
    if (option(foxygen)==on) then
        do iroom = 1, n_rooms
            roomptr => roominfo(iroom)
            p(iroom+nofoxyu)=0.23_eb*roomptr%mass(u)
            p(iroom+nofoxyl)=0.23_eb*roomptr%mass(l)
        end do
    end if

    return
    end subroutine initialize_ambient

! --------------------------- initialize_leakage -------------------------------------------
    
!> \brief   initialize specified leakage by creating vents adding up to specified area

    subroutine initialize_leakage
    
    integer iroom, counter
    real(eb) :: area
    type(room_type), pointer :: roomptr
    type(vent_type), pointer :: ventptr
    
    counter = 0
    do iroom = 1, n_rooms
        roomptr => roominfo(iroom)
        ! wall leakage
        if (roomptr%leak_area_ratios(1) /= 0.0_eb) then
            n_leaks = n_leaks + 1
            counter = counter + 1
            ventptr => leakinfo(n_leaks)
            ventptr%id = 'Wall Leak ' // trim(roomptr%id)
            ventptr%room1 = roomptr%compartment
            ventptr%room2 = n_rooms+1
            ventptr%counter = counter
            ventptr%sill   = roomptr%cheight*0.05
            ventptr%soffit = roomptr%cheight*0.95
            area = 2 * (roomptr%cwidth + roomptr%cdepth) * roomptr%cheight
            ventptr%width  = area*roomptr%leak_area_ratios(1)/(ventptr%soffit-ventptr%sill)
            ventptr%offset(1) = 0._eb
            ventptr%offset(2) = 0._eb
            ventptr%face=1
            ventptr%opening_type = trigger_by_time
            ventptr%opening_initial_time = 0._eb
            ventptr%opening_initial_fraction = 1._eb
            ventptr%opening_final_time = 0._eb
            ventptr%opening_final_fraction = 1._eb
            ventptr%absolute_soffit = ventptr%soffit + roomptr%z0
            ventptr%absolute_sill = ventptr%sill + roomptr%z0
        end if
        
        ! floor leakage
        if (roomptr%leak_area_ratios(2) /= 0.0_eb) then
            n_leaks = n_leaks + 1
            counter = counter + 1
            ventptr => leakinfo(n_leaks)
            ventptr%id = 'Floor Leak ' // trim(roomptr%id)
            ventptr%room1 = roomptr%compartment
            ventptr%room2 = n_rooms+1
            ventptr%counter = counter
            ventptr%sill   = 0._eb
            ventptr%width = 0.9_eb * (roomptr%cwidth + roomptr%cdepth)/2
            area = roomptr%cwidth * roomptr%cdepth
            ventptr%soffit  = area*roomptr%leak_area_ratios(2)/ventptr%width
            ventptr%offset(1) = 0._eb
            ventptr%offset(2) = 0._eb
            ventptr%face=1
            ventptr%opening_type = trigger_by_time
            ventptr%opening_initial_time = 0._eb
            ventptr%opening_initial_fraction = 1._eb
            ventptr%opening_final_time = 0._eb
            ventptr%opening_final_fraction = 1._eb
            ventptr%absolute_soffit = ventptr%soffit + roomptr%z0
            ventptr%absolute_sill = ventptr%sill + roomptr%z0
        end if
    end do
    
    end subroutine initialize_leakage

! --------------------------- initialize_memory -------------------------------------------

!>  initializes main model variable

    subroutine initialize_memory

    integer i
    type(room_type), pointer :: roomptr

    if (init_scalars) then
        init_scalars = .false.
        ! simple control stuff
        debugging = .false.
        jaccol = -2

        ! DASSL forcing functions
        p(1:maxteq) = 0.0_eb

        ! set time-released defaults
        deltat = 1.0_eb

        ! time step checking
        stpmin = 1.0e-09_eb
        stpmin_cnt = 0
        stpmin_cnt_max = 100
        stpminflag = .true.

        ! define universal constants

        t_ref = default_temperature
        pressure_ref = default_pressure
        interior_abs_pressure = pressure_ref
        pressure_offset = pressure_ref
        interior_ambient_temperature = t_ref
        tgignt = t_ref + 200.0_eb
        exterior_ambient_temperature = interior_ambient_temperature
        exterior_abs_pressure = interior_abs_pressure
        relative_humidity = default_relative_humidity
    end if 

    !thermal properties. initialize to nothing
    if (alloc_matl) then 
        alloc_matl = .false.
        allocate (material_info(mxmatl))
    end if
    if (init_matl) then
        init_matl = .false. 
        n_matl = 0
        material_info(1:mxmatl)%id          = ' '
        material_info(1:mxmatl)%nslab         = 1
        material_info(1:mxmatl)%k(1)          = 0.0_eb
        material_info(1:mxmatl)%c(1)          = 0.0_eb
        material_info(1:mxmatl)%rho(1)        = 0.0_eb
        material_info(1:mxmatl)%thickness(1)  = 0.0_eb
        material_info(1:mxmatl)%eps           = 0.0_eb
    end if

    ! rooms
    if (alloc_room) then
        alloc_room = .false.
        allocate (roominfo(mxrooms))
    end if
    if (init_room) then
        init_room = .false.
        n_rooms = 0
        roominfo(1:mxrooms)%id = ' '
        roominfo(1:mxrooms)%cwidth = xlrg
        roominfo(1:mxrooms)%cdepth = xlrg
        roominfo(1:mxrooms)%cheight = xlrg
        roominfo(1:mxrooms)%x0 = 0.0_eb
        roominfo(1:mxrooms)%y0 = 0.0_eb
        roominfo(1:mxrooms)%z0 = 0.0_eb
        roominfo(1:mxrooms)%x1 = xlrg
        roominfo(1:mxrooms)%y1 = xlrg
        roominfo(1:mxrooms)%z1 = xlrg
        roominfo(1:mxrooms)%ibar = default_grid
        roominfo(1:mxrooms)%jbar = default_grid
        roominfo(1:mxrooms)%kbar = default_grid
        roominfo(1:mxrooms)%deadroom = 0
        roominfo(1:mxrooms)%hall = .false.
        roominfo(1:mxrooms)%shaft = .false.
        roominfo(1:mxrooms)%sprinkler_activated = 0
        roominfo(1:mxrooms)%qdot_doorjet = 0.0_eb

        do i = 1, mxrooms
            roomptr => roominfo(i)
            roomptr%floor_area = roomptr%cwidth*roomptr%cdepth
            roomptr%cvolume = roomptr%cheight*roomptr%floor_area
            roomptr%matl(1,1:nwal) = 'OFF'
            roomptr%surface_on(1:nwal) = .false.
            roomptr%eps_w(1:nwal) = 0.0_eb
        end do

        adiabatic_walls = .false.
    
        ! room to room heat transfer
        n_vcons = 0

        do i = 1, mxrooms
            roomptr => roominfo(i)
        
            ! variable cross sectional area
            roomptr%nvars = 0
            roomptr%var_volume(1:mxpts) = 0.0_eb
            roomptr%var_area(1:mxpts) = 0.0_eb
            roomptr%var_height(1:mxpts) = 0.0_eb
        
            ! initialize inter-compartment heat transfer fractions
            roomptr%iheat = 0
            roomptr%hheat_connections(1:mxrooms) = 0
            roomptr%heat_frac(1:mxrooms) = 0
        
            !initialize surface opening fraction
            roomptr%chi(1:10) = 0._eb
        end do

        ! initialize number of furnace temperature nodes
        n_furn=0
    end if

    ! allocate vents
    if (alloc_vent) then
        alloc_vent = .false. 
        allocate (hventinfo(mxhvents))
        allocate (leakinfo(mxleaks))
        allocate (vventinfo(mxvvents))
        allocate (mventinfo(mxmvents))
    end if
    if (init_vent) then
        init_vent = .false.
        
        ! horizontal vents
        n_hvents = 0
        hventinfo(1:mxhvents)%vtype = 'H'
        hventinfo(1:mxhvents)%id = ' '
        hventinfo(1:mxhvents)%width = 0.0_eb
        hventinfo(1:mxhvents)%soffit = 0.0_eb
        hventinfo(1:mxhvents)%sill = 0.0_eb
        hventinfo(1:mxhvents)%absolute_soffit = 0.0_eb
        hventinfo(1:mxhvents)%absolute_sill = 0.0_eb
        hventinfo(1:mxhvents)%face = face_front
        ! start with vents open
        hventinfo(1:mxhvents)%opening_type = trigger_by_time
        hventinfo(1:mxhvents)%opening_triggered = .false.
        hventinfo(1:mxhvents)%opening_initial_time = 0.0_eb
        hventinfo(1:mxhvents)%opening_initial_fraction = 1.0_eb
        hventinfo(1:mxhvents)%opening_final_time = 0.0_eb
        hventinfo(1:mxhvents)%opening_final_fraction = 1.0_eb
        hventinfo(1:mxhvents)%npoints = 0
        do i = 1, mxhvents
            hventinfo(i)%t(1:mxpts) = 0._eb
            hventinfo(i)%f(1:mxpts) = 0.0_eb
        end do

        ! leakage vents
        n_leaks = 0
        leakinfo(1:mxleaks)%vtype = 'H'
        leakinfo(1:mxleaks)%id = ' '
        leakinfo(1:mxleaks)%width = 0.0_eb
        leakinfo(1:mxleaks)%soffit = 0.0_eb
        leakinfo(1:mxleaks)%sill = 0.0_eb
        leakinfo(1:mxleaks)%absolute_soffit = 0.0_eb
        leakinfo(1:mxleaks)%absolute_sill = 0.0_eb
        leakinfo(1:mxleaks)%face = face_front
        ! start with vents open
        leakinfo(1:mxleaks)%opening_type = trigger_by_time
        leakinfo(1:mxleaks)%opening_triggered = .false.
        leakinfo(1:mxleaks)%opening_initial_time = 0.0_eb
        leakinfo(1:mxleaks)%opening_initial_fraction = 1.0_eb
        leakinfo(1:mxleaks)%opening_final_time = 0.0_eb
        leakinfo(1:mxleaks)%opening_final_fraction = 1.0_eb
        leakinfo(1:mxhvents)%npoints = 0
        do i = 1, mxleaks
            leakinfo(i)%t(1:mxpts) = 0._eb
            leakinfo(i)%f(1:mxpts) = 0.0_eb
        end do

        ! vertical vents
        n_vvents = 0
        vventinfo(1:mxhvents)%vtype = 'V'
        vventinfo(1:mxvvents)%id = ' '
        vventinfo(1:mxvvents)%shape = 1
        vventinfo(1:mxvvents)%area = 0.0_eb
        ! start with vents open
        vventinfo(1:mxvvents)%opening_type = trigger_by_time
        vventinfo(1:mxvvents)%opening_triggered = .false.
        vventinfo(1:mxvvents)%opening_initial_time = 0.0_eb
        vventinfo(1:mxvvents)%opening_initial_fraction = 1.0_eb
        vventinfo(1:mxvvents)%opening_final_time = 0.0_eb
        vventinfo(1:mxvvents)%opening_final_fraction = 1.0_eb
        vventinfo(1:mxvvents)%npoints = 0
        do i = 1, mxvvents
            vventinfo(i)%t(1:mxpts) = 0._eb
            vventinfo(i)%f(1:mxpts) = 0.0_eb
        end do

        ! mechanical vents

        n_mvents = 0
        vventinfo(1:mxhvents)%vtype = 'M'
        mventinfo(1:mxmvents)%id = ' '
        mventinfo(1:mxmvents)%total_flow(u) = 0.0_eb
        mventinfo(1:mxmvents)%total_flow(l) = 0.0_eb
        mventinfo(1:mxmvents)%total_trace_flow(u) = 0.0_eb
        mventinfo(1:mxmvents)%total_trace_flow(l) = 0.0_eb
        mventinfo(1:mxmvents)%total_trace_filtered(u) = 0.0_eb
        mventinfo(1:mxmvents)%total_trace_filtered(l) = 0.0_eb
        ! note that the fan fraction is unity = on, whereas the filter fraction is unity = 100% filtering 
        mventinfo(1:mxmvents)%opening_type = trigger_by_time
        mventinfo(1:mxmvents)%opening_triggered = .false.
        mventinfo(1:mxmvents)%opening_initial_time = 0.0_eb
        mventinfo(1:mxmvents)%opening_initial_fraction = 1.0_eb
        mventinfo(1:mxmvents)%opening_final_time = 0.0_eb
        mventinfo(1:mxmvents)%opening_final_fraction = 1.0_eb
        mventinfo(1:mxmvents)%filter_initial_time = 0.0_eb
        mventinfo(1:mxmvents)%filter_initial_fraction = 0.0_eb
        mventinfo(1:mxmvents)%filter_final_time = 0.0_eb
        mventinfo(1:mxmvents)%filter_final_fraction = 0.0_eb
        mventinfo(1:mxmvents)%npoints = 0
        do i = 1, mxmvents
            mventinfo(i)%t(1:mxpts) = 0._eb
            mventinfo(i)%f(1:mxpts) = 0.0_eb
        end do
    end if 

    ! allocate devices
    if (alloc_devc) then
        alloc_devc = .false.
        allocate (detectorinfo(mxdtect))
        allocate (targetinfo(mxtarg))
    end if
    if (init_devc) then
        init_devc = .false.
        
        ! detectors
        n_detectors = 0
        detectorinfo(1:mxdtect)%id = ' '
        detectorinfo(1:mxdtect)%rti = default_rti
        detectorinfo(1:mxdtect)%spray_density = -300.0_eb
        detectorinfo(1:mxdtect)%center(1) = -1.0_eb
        detectorinfo(1:mxdtect)%center(2) = -1.0_eb
        detectorinfo(1:mxdtect)%center(3) = -3.0_eb/39.37_eb
        detectorinfo(1:mxdtect)%trigger = default_activation_temperature
        detectorinfo(1:mxdtect)%velocity = 0.0_eb
        detectorinfo(1:mxdtect)%velocity_o = 0.0_eb
        detectorinfo(1:mxdtect)%activation_time = 99999.0_eb
        detectorinfo(1:mxdtect)%dtype = 2
        detectorinfo(1:mxdtect)%room = 1
        detectorinfo(1:mxdtect)%quench = .false.
        detectorinfo(1:mxdtect)%activated = .false.
        detectorinfo(1:mxdtect)%reported = .false.

        ! targets
        n_targets = 0
        targetinfo(1:mxtarg)%id = ' '
        targetinfo(1:mxtarg)%room = 0
        targetinfo(1:mxtarg)%equaton_type = pde
        targetinfo(1:mxtarg)%back = interior
        targetinfo(1:mxtarg)%material = 'DEFAULT'
        targetinfo(1:mxtarg)%fed_gas = 0.0_eb
        targetinfo(1:mxtarg)%dfed_gas = 0.0_eb
        targetinfo(1:mxtarg)%fed_heat = 0.0_eb
        targetinfo(1:mxtarg)%dfed_heat = 0.0_eb
    end if 
    
    ! fires
    call initialize_fires
    
    ! spreadsheet output data
    if (alloc_ss) then
        alloc_ss = .false.
        allocate (sscompinfo(mxss))
        allocate (ssdeviceinfo(mxss))
        allocate (sswallinfo(mxss))
        allocate (ssmassinfo(mxss))
        allocate (ssventinfo(mxss))
    end if
    if (init_ss) then
        init_ss = .false.
        n_sscomp = 0
        n_ssdevice = 0
        n_sswall = 0
        n_ssmass = 0
        n_ssvent = 0
    end if
    
    ! post-run calculation data
    if (alloc_dump) then
        alloc_dump = .false.
        allocate (dumpinfo(mx_dumps))
    end if
    if (init_dump) then
        init_dump = .false.
        n_dumps = 0
        dumpinfo(1:mx_dumps)%file = ' '
        dumpinfo(1:mx_dumps)%type = ' '
        dumpinfo(1:mx_dumps)%first_field(1) = ' '
        dumpinfo(1:mx_dumps)%first_field(2) = ' '
        dumpinfo(1:mx_dumps)%second_field(1) = ' '
        dumpinfo(1:mx_dumps)%second_field(2) = ' '
        dumpinfo(1:mx_dumps)%relative_column = -1
        dumpinfo(1:mx_dumps)%criterion = -1
    end if

    return
    end subroutine initialize_memory
    

! --------------------------- initialize_fires -------------------------------------------
    
!> \brief   initialize fire variables

    subroutine initialize_fires
    
    integer i, lsp

    ! initializes fires

    lower_o2_limit = default_lower_oxygen_limit
    
    if (alloc_fire) then
        alloc_fire = .false.
        allocate (fireinfo(mxfires))
        allocate (tablinfo(mxtabls))
    end if
    
    ! fire data initialized to no fires
    if (init_fire) then
        init_fire = .false.
        n_fires = 0
        fireinfo(1:mxfires)%x_position = -1.0_eb
        fireinfo(1:mxfires)%y_position = -1.0_eb
        fireinfo(1:mxfires)%z_position = -1.0_eb
        fireinfo(1:mxfires)%room = 0
        fireinfo(1:mxfires)%id = ' '
        fireinfo(1:mxfires)%chemistry_type = 2
        fireinfo(1:mxfires)%ignition_type = trigger_by_time
        fireinfo(1:mxfires)%ignition_criterion = 0.0_eb
        fireinfo(1:mxfires)%ignition_time = 0.0_eb
        fireinfo(1:mxfires)%ignited = .false.
        fireinfo(1:mxfires)%reported = .false.
        fireinfo(1:mxfires)%modified_plume = 1
        fireinfo(1:mxfires)%chirad = default_radiative_fraction
        fireinfo(1:mxfires)%flaming_transition_time = 0._eb

        fireinfo(1:mxfires)%qdot_at_activation(u) = 0.0_eb
        fireinfo(1:mxfires)%qdot_at_activation(l) = 0.0_eb
        fireinfo(1:mxfires)%qdot_layers(u) = 0.0_eb
        fireinfo(1:mxfires)%qdot_layers(l) = 0.0_eb
        
        ! For CData
        fireinfo(1:mxfires)%CData_modifying_fire_flag = .false. 

        ! trace species stuff
        fireinfo(1:mxfires)%total_pyrolysate = 0.0_eb
        fireinfo(1:mxfires)%total_trace = 0.0_eb
        summed_total_trace = 0.0_eb
    
        ! no data for fires without user input
        n_tabls = 0
        tablinfo(1:mxtabls)%id = ' '
        do lsp = 1, ns+3
            tablinfo(1:mxtabls)%labels(lsp) = ' '
        end do
        do i = 1, mxpts
            do lsp = 1, ns+3
                tablinfo(1:mxtabls)%data(i,lsp) = 0.0_eb
            end do
        end do
        tablinfo(1:mxtabls)%n_points = 0
        tablinfo(1:mxtabls)%n_columns = 0
    end if

    return
    end subroutine initialize_fires

! --------------------------- initialize_species -------------------------------------------
    
!> \brief   initialize species variables

    subroutine initialize_species

    real(eb) :: xt, xtemp, xh2o, totmass, initialmass(2,mxrooms,ns)
    integer i, j, k, ip, iprod, isof, lsp
    type(room_type), pointer :: roomptr
    
    initial_mass_fraction(1:ns) = 0.0_eb

    ! normal air
    initial_mass_fraction(1) = 0.770_eb
    initial_mass_fraction(2) = 0.230_eb
    
    do i = 1, n_rooms
        roomptr => roominfo(i)
        roomptr%species_mass(u:l,1:ns) = 0.0_eb
        roomptr%species_fraction(u:l,1:ns) = 0.0_eb

        !  set the water content to relative_humidity - the polynomial fit is to (t-273), and
        ! is for saturation pressure of water.  this fit comes from the steam
        ! tables in the handbook of physics and chemistry.
        xt = interior_ambient_temperature
        xtemp = 23.2_eb - 3.816e3_eb/(xt-46.0_eb)
        xh2o = exp(xtemp)/101325.0_eb*(18.016_eb/28.584_eb)
        initial_mass_fraction(h2o) = relative_humidity*xh2o

        ! normalize the compartment atmosphere
        totmass = 0.0_eb
        do j = 1, ns_mass
            totmass = totmass + initial_mass_fraction(j)
        end do
        initial_mass_fraction(1:ns) = initial_mass_fraction(1:ns)/totmass

        do k = u, l
            do lsp = 1, ns
                roomptr%species_output(k,lsp) = 0.0_eb
                initialmass(k,i,lsp) = initial_mass_fraction(lsp)*interior_rho*roomptr%volume(k)
            end do
        end do
    end do
    
    ! initialize vent species
    do k = u, l
        do lsp = 1, ns
            hventinfo(1:mxhvents)%species_fraction(k,lsp) = 0.0_eb
            vventinfo(1:mxvvents)%species_fraction(k,lsp) = 0.0_eb
            mventinfo(1:mxmvents)%species_fraction(k,lsp) = 0.0_eb
            leakinfo(1:mxleaks)%species_fraction(k,lsp) = 0.0_eb
        end do
    end do

    isof = nofprd
    do lsp = 1, ns
        do i = 1, n_rooms
            do k = u, l
                isof = isof + 1
                p(isof) = initialmass(k,i,lsp)
            end do
        end do
    end do

    ! define product map array
    i_speciesmap(1) = 1
    i_speciesmap(2) = 2
    ip = 2
    do iprod = 1, ns
        ip = ip + 1
        i_speciesmap(ip) = iprod + 2
    end do

    return
    end subroutine initialize_species

! --------------------------- initialize_targets -------------------------------------------
    
!> \brief   initialize target variables

    subroutine initialize_targets

    real(eb) :: hypotenuse
    integer :: itarg, iroom, tp, j
    character(len=mxthrmplen) :: tcname

    type(target_type), pointer :: targptr
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr
    type(material_type), pointer :: thrmpptr

    do itarg = 1, n_targets

        ! room number must be between 1 and n_rooms
        targptr => targetinfo(itarg)
        iroom = targptr%room
        if (iroom<1.or.iroom>n_rooms) then
            write (errormessage,'(a,i0)') '***Error, Target assigned to non-existent compartment',iroom
            call cfastexit('initialize_targets',1)
            stop
        end if
        roomptr => roominfo(iroom)

        ! if the locator is -1, set to center of room on the floor
        if (targptr%center(1)==-1.0_eb) targptr%center(1) = 0.5_eb*roomptr%cwidth
        if (targptr%center(2)==-1.0_eb) targptr%center(2) = 0.5_eb*roomptr%cdepth
        if (targptr%center(3)==-1.0_eb) targptr%center(3) = 0.0_eb

        ! center coordinates need to be within room
        if (targptr%center(1)<0.0_eb.or.targptr%center(1)>roomptr%cwidth.or. &
            targptr%center(2)<0.0_eb.or.targptr%center(2)>roomptr%cdepth.or. &
            targptr%center(3)<0.0_eb.or.targptr%center(3)>roomptr%cheight) then
            write (errormessage,'(a,i0,1x,3f10.3)') '***Error, Target located outside of compartment', iroom, &
                targptr%center(1), targptr%center(2), targptr%center(3)
            call cfastexit('initialize_targets',2)
            stop
        end if

        ! set up normal vector
        if (targptr%surface_orientation == "CEILING") then
            targptr%normal = (/0._eb, 0._eb, 1._eb/)
        else if (targptr%surface_orientation == "FLOOR") then
            targptr%normal = (/0._eb, 0._eb, -1._eb/)
        else if (targptr%surface_orientation == "FRONT WALL") then
            targptr%normal = (/1._eb, 0._eb, 0._eb/)
        else if (targptr%surface_orientation == "BACK WALL") then
            targptr%normal = (/-1._eb, 0._eb, 0._eb/)
        else if (targptr%surface_orientation == "RIGHT WALL") then
            targptr%normal = (/0._eb, 1._eb, 0._eb/)
        else if (targptr%surface_orientation == "LEFT WALL") then
            targptr%normal = (/0._eb, -1._eb, 0._eb/)
        else
            do j = 1, n_fires
                fireptr => fireinfo(j)
                if (targptr%surface_orientation == fireptr%id) then
                    hypotenuse = sqrt((fireptr%x_position-targptr%center(1))**2 + &
                        (fireptr%y_position-targptr%center(2))**2 + &
                        (fireptr%height(1)-targptr%center(3))**2)
                    If (Hypotenuse /= 0._eb) Then
                        targptr%normal(1) = (fireptr%x_position - targptr%center(1)) / hypotenuse
                        targptr%normal(2) = (fireptr%y_position - targptr%center(2)) / hypotenuse
                        targptr%normal(3) = (fireptr%height(1) - targptr%center(3)) / hypotenuse
                    else
                        write(errormessage, '(a,i3)') &
                            '***Error in &DEVC: Invalid specification for normal vector. Check &DEVC input, ' , itarg
                        call cfastexit('initialize_targets',3)
                        stop
                    end if
                end if
            end do
        end if

        ! set up target thermal properties
        tcname = targptr%material
        if (tcname==' ') then
            tcname = 'DEFAULT'
            targptr%material = tcname
        end if
        call get_thermal_property(tcname,tp)
        thrmpptr => material_info(tp)
        targptr%k = thrmpptr%k(1)
        targptr%c = thrmpptr%c(1)
        targptr%rho = thrmpptr%rho(1)
        if (targptr%thickness<=0._eb) targptr%thickness = thrmpptr%thickness(1)
        if (targptr%depth_units=='FRACTION') then
            targptr%depth_loc = max(0.0_eb,min(targptr%thickness*targptr%depth_loc,targptr%thickness))
        else
            targptr%depth_loc = max(0.0_eb,min(targptr%depth_loc,targptr%thickness))
        end if
        targptr%emissivity = thrmpptr%eps
    end do

    return
    end subroutine initialize_targets

! --------------------------- initialize_walls  -------------------------------------------

!> \brief   initializes data structures associated with walls and targets

!> \param   tstop (input): total simulation time. used to estimate a characteristic thermal penetration time 
!>                         and ensure the explicit calculation will converge

    subroutine initialize_walls

    ! kw = thermal conductivity
    ! cw = specific heat (j/kg)
    ! rhow = density of the wall (kg/m**3)
    ! thickw = thickness of the wall (m)
    ! epsw = emmisivity of the wall
    ! nslb = discretization of the wall slabs (number of nodes)
    ! matl contains the name of the thermal data set in the tpp data structure
    ! n_matl is a count of the number of tpp data sets in the tpp data structure

    integer :: i, j, jj, k, ifromr, itor, ifromw, itow, nslabf, nslabt, nptsf, nptst, wfrom, wto
    real(eb) :: k_w(mxslb), c_w(mxslb), rho_w(mxslb), thick_w(mxslb), thick, wtemps(nnodes), walldx(nnodes)
    integer nslab, n_nodes(mxslb+1)
    character(len=mxthrmplen) :: off = 'OFF', none = 'NONE'

    ! tp is the pointer into the data base for each material
    integer tp

    type(room_type), pointer :: roomptr, from_roomptr, to_roomptr
    type(material_type), pointer :: thrmpptr

    ! map the thermal data into its appropriate wall specification
    ! if name is "OFF" or "NONE" then just turn all off
    do i = 1, nwal
        do j = 1, n_rooms
            roomptr => roominfo(j)
            if (roomptr%surface_on(i)) then
                if (roomptr%matl(1,i)==off.or.roomptr%matl(1,i)==none) then
                    roomptr%surface_on(i) = .false.
                else
                    do k = 1, roomptr%nslab_w(i)
                        call get_thermal_property(roomptr%matl(k,i),tp)
                        thrmpptr => material_info(tp)
                        if (k==1) roomptr%eps_w(i) = thrmpptr%eps
                        roomptr%k_w(k,i) = thrmpptr%k(1)
                        roomptr%c_w(k,i) = thrmpptr%c(1)
                        roomptr%rho_w(k,i) = thrmpptr%rho(1)
                        if (roomptr%thick_w(k,i)==0.0_eb) then
                            roomptr%thick_w(k,i) = thrmpptr%thickness(1)
                        end if
                    end do
                end if
            end if
        end do
    end do

    ! initialize temperature profile data structures
    do i = 1, n_rooms
        roomptr => roominfo(i)
        do j = 1, nwal
            roomptr%t_profile(1:nnodes,j) = interior_ambient_temperature
            if (roomptr%surface_on(j)) then
                k_w(1:mxslb) = roomptr%k_w(1:mxslb,j)
                c_w(1:mxslb) = roomptr%c_w(1:mxslb,j)
                rho_w(1:mxslb) = roomptr%rho_w(1:mxslb,j)
                thick_w(1:mxslb) = roomptr%thick_w(1:mxslb,j)
                nslab = roomptr%nslab_w(j)
                thick = roomptr%total_thick_w(j)
                n_nodes = roomptr%nodes_w(1:mxslb+1,j)
                wtemps = roomptr%t_profile(1:nnodes,j)
                walldx = roomptr%walldx(1:nnodes,j)
                call initialize_wall_nodes(n_nodes,nslab,walldx,slab_splits,k_w,c_w,rho_w,thick_w, &
                   thick,wtemps,interior_ambient_temperature,exterior_ambient_temperature)
                roomptr%nodes_w(1:mxslb+1,j) = n_nodes
                roomptr%t_profile(1:nnodes,j) = wtemps
                roomptr%walldx(1:nnodes,j) = walldx
                roomptr%total_thick_w(j) = thick
            end if
        end do
    end do

    ! concatenate slab properties of wall nodes that are connected to each other
    do i = 1, n_vcons
        ifromr = vertical_connections(i,w_from_room)
        ifromw = vertical_connections(i,w_from_wall)
        itor = vertical_connections(i,w_to_room)
        itow = vertical_connections(i,w_to_wall)
        from_roomptr => roominfo(ifromr)
        to_roomptr => roominfo(itor)

        nslabf = from_roomptr%nslab_w(ifromw)
        nslabt = to_roomptr%nslab_w(itow)
        from_roomptr%nslab_w(ifromw) = nslabf + nslabt
        to_roomptr%nslab_w(itow) = nslabf + nslabt

        nptsf = from_roomptr%nodes_w(1,ifromw)
        nptst = to_roomptr%nodes_w(1,itow)
        to_roomptr%nodes_w(1,itow) = nptsf + nptst - 1
        from_roomptr%nodes_w(1,ifromw) = nptsf + nptst - 1

        wfrom = from_roomptr%total_thick_w(ifromw)
        wto = to_roomptr%total_thick_w(itow)
        from_roomptr%total_thick_w(ifromw) = wfrom + wto
        to_roomptr%total_thick_w(itow) = wfrom + wto

        jj = nslabt + 1
        do j = nslabf+1, nslabf+nslabt
            jj = jj - 1
            from_roomptr%k_w(j,ifromw) = to_roomptr%k_w(jj,itow)
            from_roomptr%c_w(j,ifromw) = to_roomptr%c_w(jj,itow)
            from_roomptr%rho_w(j,ifromw) = to_roomptr%rho_w(jj,itow)
            from_roomptr%thick_w(j,ifromw) = to_roomptr%thick_w(jj,itow)
            from_roomptr%nodes_w(j+1,ifromw) = to_roomptr%nodes_w(jj+1,itow)
        end do

        jj = nslabf + 1
        do j = nslabt+1, nslabt+nslabf
            jj = jj - 1
            to_roomptr%k_w(j,itow) = from_roomptr%k_w(jj,ifromw)
            to_roomptr%c_w(j,itow) = from_roomptr%c_w(jj,ifromw)
            to_roomptr%rho_w(j,itow) = from_roomptr%rho_w(jj,ifromw)
            to_roomptr%thick_w(j,itow) = from_roomptr%thick_w(jj,ifromw)
            to_roomptr%nodes_w(j+1,itow) = from_roomptr%nodes_w(jj+1,ifromw)
        end do

        do j = 1,nptsf
            from_roomptr%t_profile(j,ifromw) = interior_ambient_temperature
            to_roomptr%t_profile(j,itow) = interior_ambient_temperature
        end do
        jj = nptst
        do j = nptsf+1,nptsf+nptst - 1
            jj = jj - 1
            from_roomptr%t_profile(j,ifromw) = interior_ambient_temperature
            from_roomptr%walldx(j-1,ifromw) = to_roomptr%walldx(jj,itow)
        end do

        jj = nptsf
        do j = nptst+1,nptst+nptsf - 1
            jj = jj - 1
            to_roomptr%t_profile(j,itow) = interior_ambient_temperature
            to_roomptr%walldx(j-1,itow) = from_roomptr%walldx(jj,ifromw)
        end do
    end do

    return
    end subroutine initialize_walls

! --------------------------- initialize_wall_nodes -------------------------------------------

!> \brief   initialize temperature profiles, breakpoints used in wall conduction calculations
    
!> \param   n_nodes (input): number of nodes in each slab
!> \param   nslab (input): number of slabs
!> \param   tstop (input): final simulation time
!> \param   slab_splits (input): fraction of points assigned to slabs 1, 2 and 3
!> \param   wk (input): wall thermal conductivity
!> \param   wspec (input): wall specific heat
!> \param   wrho (input): wall density
!> \param   slab_thickness (input): thickness of each slab
!> \param   tamb (input): ambient temperature seen by interior wall
!> \param   text (input): ambient temperature seen by exterior wall
!> \param   wall_thickness (output): total thickness of wall
!> \param   wtemp (output): wall temperature profile
!> \param   walldx (output): wall node positions

    subroutine initialize_wall_nodes (n_nodes, nslab, walldx, slab_splits, wk, wspec, wrho, slab_thickness, &
        wall_thickness, wtemp, tamb, text)

    integer, intent(in) :: nslab
    real(eb), intent(in) :: slab_splits(*), wk(*), wspec(*), wrho(*), slab_thickness(*), tamb, text
    integer, intent(inout) :: n_nodes(*)
    real(eb), intent(out) :: wall_thickness, walldx(*)

    integer :: cumpts(10), numpts(10), i, ii, nx, nintx, nsplit, islab, isum, nint, ibeg, iend
    real(eb) :: wtemp(*), xwall(100), xpos(10), xxnx, errfc05, xkrhoc, alpha, xb, xxnsplit, w, xxim1, xxiim1
    real(eb) :: wmxb, xxnslabm2, xxnint, xxi1, xxi2, xxi3, xxnintx, dtdw

    nx = n_nodes(1)
    xxnx = nx

    nintx = nx - (nslab+1)
    if (nslab<=2) then
        nsplit = (slab_splits(1)+slab_splits(2))*xxnx
    else
        nsplit = slab_splits(1)*xxnx
    end if

    ! calculate total walldepth
    xpos(1) = 0.0_eb
    do islab = 1, nslab
        xpos(islab+1) = xpos(islab) + slab_thickness(islab)
    end do
    wall_thickness = xpos(nslab+1)

    ! calculate break point based on first slab's properties
    errfc05 = 1.30_eb
    xkrhoc = wk(1)/(wspec(1)*wrho(1))
    alpha = sqrt(xkrhoc)
    !xb = 2.0_eb*alpha*sqrt(tstop)*errfc05*wall_thickness
    xb = 0.5_eb*wall_thickness
    if (xb>0.50_eb*wall_thickness) xb = 0.5_eb*wall_thickness
    if (nslab==1) then

        ! set up wall node locations for 1 slab case, bunch points at interior and exterior boundary
        xxnsplit = nsplit
        w = 1.0_eb/xxnsplit
        do i = 1, nsplit + 1
            xxim1 = i - 1
            xwall(i) = xb*(xxim1*w)**2
        end do
        w = 1.0_eb/(xxnx-(xxnsplit+1.0_eb))
        do i = nsplit +2, nx
            ii = nx + 1 - i
            xxiim1 = ii - 1
            xwall(i) = wall_thickness - (wall_thickness-xb)*(xxiim1*w)**2
        end do
        n_nodes(1+nslab) = nintx
    else

        ! set up wall node locations for multi-slab case, bunch points at interior boundary of first slab, exterior
        ! boundary of last slab and uniformly in middle slabs

        ! calculate number of points interior to each slab
        xxnintx = nintx
        numpts(1) = slab_splits(1)*xxnintx*min(xb,slab_thickness(1))/wall_thickness
        if (numpts(1)<1) numpts(1) = 1
        wmxb = wall_thickness - xb
        numpts(nslab) = slab_splits(3)*xxnintx*min(wmxb,slab_thickness(nslab))/ wall_thickness
        if (numpts(nslab)<1) numpts(nslab) = 1
        isum = nintx - numpts(1) - numpts(nslab)
        xxnslabm2 = nslab - 2
        do i = 2, nslab - 1
            numpts(i) = xxnx*slab_splits(2)*slab_thickness(nslab)/xxnslabm2/wall_thickness
            if (numpts(i)<1) numpts(i) = 1
            isum = isum - numpts(i)
        end do
        numpts(1) = numpts(1) + (isum-isum/2)
        numpts(nslab) = numpts(nslab) + isum/2
        if (numpts(nslab)<1) then
            numpts(1) = numpts(1) + numpts(nslab) - 1
            numpts(nslab) = 1
        end if

        ! copy numpts data into n_nodes and keep a running total
        cumpts(1) = 1
        do islab = 1, nslab
            n_nodes(1+islab) = numpts(islab)
            cumpts(islab+1) = cumpts(islab) + numpts(islab) + 1
        end do

        ! calculate wall positions for first slab (bunched near left)
        nint = numpts(1) + 1
        xxnint = nint
        do i = 1, nint
            xxim1 = i - 1
            xwall(i) = xxim1**2*xpos(2)/xxnint**2
        end do

        ! calculate wall positions for middle slabs (uniform)
        do islab = 2, nslab - 1
            ibeg = cumpts(islab)
            iend = cumpts(islab+1) - 1
            xxi3 = iend+1-ibeg
            do i = ibeg, iend
                xxi1 = iend+1-i
                xxi2 = i-ibeg
                xwall(i) = (xpos(islab)*xxi1+xpos(islab+1)*xxi2)/xxi3
            end do
        end do

        ! calculate wall positions for last slab (bunched near right)
        if (nslab>=2) then
            ibeg = cumpts(nslab)
            iend = cumpts(nslab+1) ! include last point for last slab
            xxi3 = iend - ibeg
            do i = ibeg, iend
                xxi1 = iend - i
                xwall(i) = xpos(nslab+1) - xxi1**2*(xpos(nslab+1) - xpos(nslab))/xxi3**2
            end do
        end if
    end if

    ! finally calculate distances between each point these distances are used by conductive_flux to setup
    ! discretization tri-diagonal matrix
    do i = 1, nx - 1
        walldx(i) = xwall(i+1) - xwall(i)
    end do

    ! initialize temperature profile.  note, wtemp(1)=wtemp(2) and wtemp(nx)=wtemp(nx-1) so dassl will think that no heat
    ! transfer needs to occur to the wall (since dt/dx=0 here)
    wtemp(1) = tamb
    wtemp(nx) = text
    dtdw = (text-tamb)/(xwall(nx-1)-xwall(2))
    do i = 2, nx-1
        wtemp(i) = tamb + (xwall(i)-xwall(2))*dtdw
    end do
    return
    end subroutine initialize_wall_nodes

! --------------------------- initialize_solver_vector -------------------------------------------
    
!> \brief   set up pointers to each variable type in the main solver array

    subroutine initialize_solver_vector ()

    ! offset in the following context is the beginning of the vector for that particular variable minus one.
    !   thus, the actual pressure array goes from nofp+1 to nofp+n_rooms.  the total number of equations to be considered
    !   is nequals, and is the last element in the last vector. each physical interface routine is responsible for
    !   the count of the number of elements in the vector for which it is resonsible.

    ! this set of parameters is set by nputp and is kept in the environment module cenviro.
    ! to index a variable, the list is something like (for temperature in this case)

    ! noftu+1, noftu+n_rooms

    ! the structure of the solver array is

    ! nofp = offset for the main pressure; the array of base pressures for each compartment
    ! noftu = upper layer temperature
    ! nofvu = upper layer volume
    ! noftl = lower layer temperature
    ! nofwt = wall surface temperatures (equivalent to the number of profiles)
    ! nofprd = species
    ! nequals = last element in the array.

    ! the arrays which use this structure are vatol, vrtol, p, pdold, pprime and pdzero

    ! an important note - solve_simulation sets the last variable to be solved to nofprd which is the
    ! beginning of the species (-1) and the end of the array which is presently used by dassl

    integer :: i, j, noxygen
    type(room_type), pointer :: roomptr

    ! count the number of walls
    n_cons = 0
    do i = 1, n_rooms
        roomptr => roominfo(i)
        do j = 1, nwal
            if (roomptr%surface_on(j)) then
                n_cons = n_cons + 1
            end if
            if (nwpts/=0) roomptr%nodes_w(1,j) = nwpts
        end do
    end do
    
    ! set number of implicit oxygen variables
    if (option(foxygen)==on) then
        noxygen = n_rooms
    else
        noxygen = 0
    end if

    ! now do all the equation offsets
    nofp = 0
    noftu = nofp + n_rooms
    nofvu = noftu + n_rooms
    noftl = nofvu + n_rooms
    nofoxyl = noftl + n_rooms
    nofoxyu = nofoxyl + noxygen
    nofwt = nofoxyu + noxygen
    nofprd = nofwt + n_cons
    nequals = nofprd + 2*n_rooms*ns

    return
    end subroutine initialize_solver_vector

end module initialization_routines
