    module namelist_input_routines

    use precision_parameters

    use exit_routines, only: cfastexit
    use fire_routines, only: flame_height
    use initialization_routines, only: initialize_memory

    use cfast_types, only: detector_type, fire_type, ramp_type, room_type, table_type, target_type, material_type, &
        vent_type, visual_type, dump_type
    
    use cparams, only: mxdtect, mxfires, mxhvents, mxvvents, mxramps, mxrooms, mxtarg, mxmvents, mxtabls, mxtablcols, &
        mxmatl, mx_hsep, default_grid, pde, cylpde, smoked, heatd, sprinkd, trigger_by_time, trigger_by_temp, trigger_by_flux, &
        w_from_room, w_to_room, w_from_wall, w_to_wall, mx_dumps
    use defaults, only: default_version, default_simulation_time, default_print_out_interval, default_smv_out_interval, &
        default_ss_out_interval, default_temperature, default_pressure, default_relative_humidity, default_lower_oxygen_limit, &
        default_sigma_s, default_activation_temperature, default_activation_obscuration, default_rti, default_stpmax, &
        default_min_cutoff_relp, default_max_cutoff_relp
    
    use devc_data, only: n_targets, targetinfo, n_detectors, detectorinfo, init_devc
    use diag_data, only: rad_solver, partial_pressure_h2o, partial_pressure_co2, gas_temperature, upper_layer_thickness, &
        verification_time_step, verification_fire_heat_flux, radi_radnnet_flag, verification_ast, &
        radiative_incident_flux_ast, radi_verification_flag
    use dump_data, only: n_dumps, dumpinfo, num_csvfiles, csvnames
    use fire_data, only: n_fires, fireinfo, n_furn, furn_time, furn_temp, tgignt, lower_o2_limit, mxpts, sigma_s, n_tabls, &
        tablinfo, init_fire
    use namelist_data, only: input_file_line_number, input_file_line, headflag, timeflag, initflag, miscflag, matlflag, &
        compflag, devcflag, rampflag, tablflag, insfflag, fireflag, ventflag, connflag, diagflag, slcfflag, isofflag, &
        dumpflag, convert_negative_distances
    use option_data, only: option, on, off, ffire, fhflow, fvflow, fmflow, fentrain, fcjet, fdfire, frad, fconduc, fconvec, &
        fdebug, fkeyeval, fpsteady, fpdassl, fgasabsorb, fresidprn, flayermixing
    use ramp_data, only: n_ramps, rampinfo
    use room_data, only: n_rooms, roominfo, exterior_ambient_temperature, interior_ambient_temperature, exterior_abs_pressure, &
        interior_abs_pressure, pressure_ref, pressure_offset, exterior_rho, interior_rho, n_vcons, vertical_connections, &
        relative_humidity, adiabatic_walls
    use setup_data, only: iofili, iofill, cfast_version, title, time_end, &
        print_out_interval, smv_out_interval, ss_out_interval, validation_flag, overwrite_testcase, inputfile, errormessage
    use solver_data, only: stpmax, stpmin, stpmin_cnt_max, stpminflag
    use smkview_data, only: n_visual, visualinfo
    use material_data, only: n_matl, material_info
    use vent_data, only: n_hvents, hventinfo, n_vvents, vventinfo, n_mvents, mventinfo, n_leaks, leakinfo, init_vent

    implicit none
    
    private

    public namelist_input, read_misc, checkread, cdata_preprocessor_rereadinputfile

    contains

    ! --------------------------- namelist_input ----------------------------------

!> \brief   read and interpret each of the namelists in the input file

    subroutine namelist_input

    implicit none

    call read_head (iofili)
    call read_time (iofili)
    call read_init (iofili)
    call read_misc (iofili)
    call read_matl (iofili)
    call read_ramp (iofili)
    call read_comp (iofili)
    call read_devc (iofili)
    call read_tabl (iofili)
    call read_fire (iofili)
    call read_chem (iofili)
    call read_vent (iofili)
    call read_conn (iofili)
    call read_isof (iofili)
    call read_slcf (iofili)
    call read_diag (iofili)
    call read_dump (iofili)

    close (iofili)
    
    return

    ! read format list
5050 format ('***Error, Error opening the input file = ',I6)


    end subroutine namelist_input

    ! --------------------------- read_head --------------------------------------
    
!> \brief   read in &HEAD namelist that includes model version and title for the run
    
!> \param   lu (input): logical input unit number for the open input file

    subroutine read_head (lu)

    integer :: ios, version
    integer, intent(in) :: lu

    namelist /HEAD/ version, title

    ios = 1
    version = 0

    rewind (unit=lu)
    input_file_line_number = 0

    ! scan entire file to look for &HEAD input
    head_loop: do
        call checkread ('HEAD', lu, ios)
        if (ios==0) headflag=.true.
        if (ios==1) then
            exit head_loop
        end if
        read(lu,HEAD,iostat=ios)
        if (ios>0) then
            write(errormessage, '(a)') '***Error in &HEAD: Invalid specification for inputs.'
            call cfastexit('read_head',1)
        end if
    end do head_loop

    if (.not.headflag) then
        write (errormessage,'(a)') '***Error, &HEAD inputs are required.'
        call cfastexit('read_head',2)
    end if

    ! we found one. read it (only the first one counts; others are ignored)
    head_flag: if (headflag) then

        rewind (lu)
        input_file_line_number = 0

        call checkread('HEAD',lu,ios)
        call set_defaults
        read(lu,HEAD)

        version = version/1000

    end if head_flag

    if (version/=cfast_version/1000) then
        write (*,5002) version, cfast_version/1000
        write (iofill,5002) version, cfast_version/1000
    end if

5002 format ('***Warning: Input file written for CFAST version', i3, ' running on CFAST version', i3)

    contains

    subroutine set_defaults

    version = default_version

    end subroutine set_defaults

    end subroutine read_head


    ! --------------------------- read_time -------------------------------------------
    
!> \brief   read in &TIME namelist that includes simulation time and output intervals
    
!> \param   lu (input): logical input unit number for the open input file
    
    subroutine read_time (lu)

    integer :: ios
    integer, intent(in) :: lu

    real(eb) :: simulation,print,spreadsheet,smokeview
    namelist /TIME/ print,simulation,spreadsheet,smokeview

    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0

    ! scan entire file to look for &TIME input
    time_loop: do
        call checkread ('TIME',lu,ios)
        if (ios==0) timeflag=.true.
        if (ios==1) then
            exit time_loop
        end if
        read(lu,TIME,iostat=ios)
        if (ios>0) then
            write(errormessage, '(a)') '***Error in &TIME: Invalid specification for inputs.'
            call cfastexit('read_time',1)
        end if
    end do time_loop

    if (.not.timeflag) then
        write (errormessage, '(/, "***Error, &TIME inputs are required.")')
        call cfastexit('read_time',2)
    end if

    ! we found one. read it (only the first one counts; others are ignored)
    time_flag: if (timeflag) then

        rewind (lu)
        input_file_line_number = 0

        call checkread('TIME',lu,ios)
        call set_defaults
        read(lu,TIME)

        time_end=simulation
        print_out_interval=print
        smv_out_interval=smokeview
        ss_out_interval=spreadsheet

    end if time_flag

    contains

    subroutine set_defaults

    simulation              = default_simulation_time    ! s
    print                   = default_print_out_interval ! s
    smokeview               = default_smv_out_interval   ! s
    spreadsheet             = default_ss_out_interval    ! s

    end subroutine set_defaults

    end subroutine read_time

    ! --------------------------- read_init ------------------------------------------
    
!> \brief   read in &INIT namelist that includes ambient conditions
    
!> \param   lu (input): logical input unit number for the open input file
    
    subroutine read_init (lu)

    integer :: ios
    integer, intent(in) :: lu

    real(eb) :: pressure
    real(eb) :: interior_temperature, exterior_temperature
    namelist /INIT/ pressure, relative_humidity, interior_temperature, exterior_temperature

    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0

    ! Scan entire file to look for &INIT input
    init_loop: do
        call checkread ('INIT',lu,ios)
        if (ios==0) initflag=.true.
        if (ios==1) then
            exit init_loop
        end if
        read(lu,INIT,err=34,iostat=ios)
34      if (ios>0) then
            write(errormessage, '(a)') '***Error in &INIT: Invalid specification for inputs.'
            call cfastexit('read_init',1)
        end if
    end do init_loop

    init_flag: if (initflag) then

        rewind (lu)
        input_file_line_number = 0

        call checkread('INIT',lu,ios)
        call set_defaults
        read(lu,INIT)

        exterior_ambient_temperature  = exterior_temperature + kelvin_c_offset
        interior_ambient_temperature  = interior_temperature + kelvin_c_offset
        exterior_abs_pressure = pressure
        relative_humidity     = relative_humidity*0.01_eb

        tgignt = interior_ambient_temperature + 200.0_eb

    end if init_flag

    contains

    subroutine set_defaults

    exterior_temperature     = default_temperature - kelvin_c_offset    ! C
    interior_temperature     = default_temperature - kelvin_c_offset    ! C
    pressure                 = default_pressure                         ! Pa
    relative_humidity        = default_relative_humidity*100._eb        ! %

    end subroutine set_defaults

    end subroutine read_init


    ! --------------------------- read_misc -------------------------------------------
    
!> \brief   read in &MISC namelist that includes various special inputs
    
!> \param   lu (input): logical input unit number for the open input file
    
    subroutine read_misc (lu)

    integer, intent(in) :: lu

    integer :: ios

    real(eb) :: max_time_step, lower_oxygen_limit
    real(eb), dimension(2) :: specific_extinction
    logical :: adiabatic, overwrite
    namelist /MISC/ adiabatic, max_time_step, lower_oxygen_limit, specific_extinction, overwrite

    ios = 1

    rewind (unit=lu) ; input_file_line_number = 0

    ! Scan entire file to look for 'MISC'
    misc_loop: do
        call checkread ('MISC',lu,ios)
        if (ios==0) miscflag=.true.
        if (ios==1) then
            exit misc_loop
        end if
        read(lu,MISC,iostat=ios)
        if (ios>0) then
            write(errormessage, '(a)') '***Error in &MISC: Invalid specification for inputs.'
            call cfastexit('read_misc',1)
        end if
    end do misc_loop

    misc_flag: if (miscflag) then

        rewind (lu)
        input_file_line_number = 0

        call checkread('MISC',lu,ios)
        call set_defaults
        read(lu,MISC)

        adiabatic_walls=adiabatic
        stpmax = max_time_step
        lower_o2_limit = lower_oxygen_limit
        sigma_s = specific_extinction
        overwrite_testcase = overwrite

    end if misc_flag

    contains

    subroutine set_defaults

    ! note most default values are set in initialize_memory and used here to initialize namelist

    overwrite = .true.
    adiabatic = .false.
    max_time_step = stpmax
    lower_oxygen_limit = default_lower_oxygen_limit
    specific_extinction = default_sigma_s

    end subroutine set_defaults

    end subroutine read_misc


    ! --------------------------- read_matl -------------------------------------------
    
!> \brief   read in &MATL namelist that includes material thermal properties
    
!> \param   lu (input): logical input unit number for the open input file
    
    subroutine read_matl (lu)


    integer :: ios,ii
    integer, intent(in) :: lu
    type(material_type), pointer :: thrmpptr

    real(eb) :: conductivity, density, emissivity, specific_heat, thickness
    character(len=64) :: id, material
    character(len=128) :: fyi
    namelist /MATL/ conductivity, density, emissivity, id, material, specific_heat, thickness, fyi

    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0

    ! Scan entire file to look for 'MATL'
    n_matl = 0
    matl_loop: do
        call checkread ('MATL',lu,ios)
        if (ios==0) matlflag=.true.
        if (ios==1) then
            exit matl_loop
        end if
        read(lu,MATL,iostat=ios)
        n_matl = n_matl + 1
        if (ios>0) then
            write(errormessage, '(a,i0)') '***Error in &MATL: Invalid specification for inputs. Check &MATL input, ' , n_matl
            call cfastexit('read_matl',1)
        end if
    end do matl_loop

    if (n_matl>mxmatl) then
        write (errormessage,'(a,i0)') '***Error, Too many thermal properties in input data file. Limit is ', mxmatl
        call cfastexit('read_matl',2)
    end if

    matl_flag: if (matlflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_matl_loop: do ii=1,n_matl

            thrmpptr => material_info(ii)

            call checkread('MATL',lu,ios)
            call set_defaults
            read(lu,MATL)

            if (.not.newid(id)) then
                write(errormessage,'(a48,a64)') '****Error, Not a unique identifier for material ',trim(id)
                call cfastexit('read_matl', 3)
            end if
            thrmpptr%id            = id
            thrmpptr%material      = material 
            thrmpptr%fyi           = fyi
            thrmpptr%nslab         = 1
            thrmpptr%k(1)          = conductivity
            thrmpptr%c(1)          = specific_heat*1e3
            thrmpptr%rho(1)        = density
            thrmpptr%thickness(1)  = thickness
            thrmpptr%eps           = emissivity

        end do read_matl_loop

    end if matl_flag


    contains

    subroutine set_defaults

    specific_heat          = 0.0_eb        !j/kg-k
    emissivity             = 0.9_eb
    conductivity           = 0.0_eb        !w/m-k
    id                     = 'NULL'
    material               = 'NULL' 
    fyi                    = ' '
    density                = 0.0_eb        !kg/m3
    thickness              = 0.0_eb        !m

    end subroutine set_defaults

    end subroutine read_matl


    ! --------------------------- read_comp -------------------------------------------
    
!> \brief   read in &COMP namelist that includes compartment specifications
    
!> \param   lu (input): logical input unit number for the open input file
    
    subroutine read_comp (lu)

    integer, intent(in) :: lu
    
    integer :: ios, i, k

    type(room_type), pointer :: roomptr

    integer,dimension(3) :: grid
    real(eb) :: depth, height ,width
    real(eb), dimension(3) :: origin
    real(eb), dimension(2) :: leak_area_ratio, leak_area
    real(eb), dimension(mxpts) :: cross_sect_areas, cross_sect_heights
    real(eb), dimension(3) :: ceiling_thickness, floor_thickness, wall_thickness
    logical :: hall, shaft
    character(len=64) :: id
    character(len=64), dimension(3) :: ceiling_matl_id, floor_matl_id, wall_matl_id
    character(len=128) :: fyi
    namelist /COMP/ cross_sect_areas, cross_sect_heights, depth, grid, hall, height, id, fyi, &
        ceiling_matl_id, floor_matl_id, wall_matl_id,ceiling_thickness, floor_thickness, wall_thickness, &
        origin, shaft, width, leak_area_ratio, leak_area

    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0

    ! Scan entire file to look for 'COMP' to make sure there is at least one compartment and not too many for the software
    comp_loop: do
        call checkread('COMP',lu,ios)
        if (ios==0) compflag=.true.
        if (ios==1) then
            exit comp_loop
        end if
        read(lu,COMP,iostat=ios)
        if (ios>0) then
            write(errormessage, '(a,i0)') '***Error in &COMP: Invalid specification for inputs. Check &COMP input, ' , n_rooms
            call cfastexit('read_comp',1)
        end if
        n_rooms = n_rooms + 1
    end do comp_loop

    if (n_rooms>mxrooms) then
        write (errormessage,'(a,i0)') '***Error, Too many compartments in input data file. Limit is ', mxrooms
        call cfastexit('read_comp',2)
    end if

    if (.not.compflag) then
        write (errormessage, '(a)')'***Error, No &COMP inputs found. &COMP inputs are required.'
        call cfastexit('read_comp',3)
    end if

    comp_flag: if (compflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_comp_loop: do i = 1, n_rooms

            roomptr => roominfo(i)

            call checkread('COMP',lu,ios)
            call set_defaults
            read(lu,COMP)

            roomptr%nvars = 0
            roomptr%var_area = 0.0_eb
            roomptr%var_height = 0.0_eb
            do k = 1, mxpts
                if (cross_sect_areas(k)/=-1001._eb) then
                    roomptr%nvars = roomptr%nvars + 1
                    roomptr%var_area(roomptr%nvars) = cross_sect_areas(k)
                    roomptr%var_height(roomptr%nvars) = cross_sect_heights(k)
                end if
            end do

            if (.not.newid(id)) then
                write(errormessage,'(a)') '**** Error All IDS in input file must be unique. &COMP id not unique ', trim(id)
                call cfastexit('read_comp',4)
            end if

            roomptr%compartment    = i
            roomptr%id      = id
            roomptr%fyi     = fyi
            roomptr%cwidth  = width
            roomptr%cdepth  = depth
            roomptr%cheight = height
            roomptr%x0      = origin(1)
            roomptr%y0      = origin(2)
            roomptr%z0      = origin(3)

            roomptr%nslab_w = 0
            ! ceiling
            do k = 1, 3
                if (trim(ceiling_matl_id(k))/='OFF'.and.trim(ceiling_matl_id(k))/='NULL') then
                    roomptr%surface_on(1) = .true.
                    roomptr%matl(k,1) = ceiling_matl_id(k)
                    roomptr%thick_w(k,1) = ceiling_thickness(k)
                    roomptr%nslab_w(1) = roomptr%nslab_w(1) + 1
                end if
            end do

            ! floor
            do k = 1, 3
                if (trim(floor_matl_id(k))/='OFF'.and.trim(floor_matl_id(k))/='NULL') then
                    roomptr%surface_on(2) = .true.
                    roomptr%matl(k,2) = floor_matl_id(k)
                    roomptr%thick_w(k,2) = floor_thickness(k)
                    roomptr%nslab_w(2) = roomptr%nslab_w(2) + 1
                end if
            end do

                ! walls
            do k = 1, 3
                if (trim(wall_matl_id(k))/='OFF'.and.trim(wall_matl_id(k))/='NULL') then
                    roomptr%surface_on(3) = .true.
                    roomptr%matl(k,3) = wall_matl_id(k)
                    roomptr%thick_w(k,3) = wall_thickness(k)
                    roomptr%nslab_w(3) = roomptr%nslab_w(3) + 1
                    roomptr%surface_on(4) = .true.
                    roomptr%matl(k,4) = wall_matl_id(k)
                    roomptr%thick_w(k,4) = wall_thickness(k)
                    roomptr%nslab_w(4) = roomptr%nslab_w(4) + 1
                end if
            end do

            roomptr%ibar = grid(1)
            roomptr%jbar = grid(2)
            roomptr%kbar = grid(3)

            roomptr%shaft=.false.
            roomptr%hall=.false.
            roomptr%shaft = shaft
            roomptr%hall = hall
            
            ! leakage
            roomptr%leak_area_ratios = leak_area_ratio
            roomptr%leak_areas = leak_area

        end do read_comp_loop

    end if comp_flag

    contains

    subroutine set_defaults

    ceiling_matl_id         = 'NULL'
    wall_matl_id            = 'NULL'
    floor_matl_id           = 'NULL'
    ceiling_thickness       = 0.0_eb
    wall_thickness          = 0.0_eb
    floor_thickness         = 0.0_eb
    cross_sect_areas        = -1001._eb
    cross_sect_heights      = -1001._eb
    id                      = 'NULL'
    fyi                     = 'NULL'
    depth                   = 0.0_eb
    height                  = 0.0_eb
    width                   = 0.0_eb
    grid(:)                 = default_grid
    origin(:)               = 0.0_eb
    leak_area_ratio(:)      = 0.0_eb
    leak_area(:)            = 0.0
    hall                    = .false.
    shaft                   = .false.

    end subroutine set_defaults

    end subroutine read_comp


    ! --------------------------- read_devc -------------------------------------------
    
!> \brief   read in &DEVC namelist that includes target and detector specifications
    
!> \param   lu (input): logical input unit number for the open input file

    subroutine read_devc (lu)

    integer, intent(in) :: lu
    
    integer :: ios
    integer :: iroom, ii, jj ,i1, counter1, counter2
    character(len=64) :: compartment_id
    character(len=64) :: tcname
    logical :: idcheck

    type(room_type), pointer :: roomptr
    type(target_type), pointer :: targptr
    type(detector_type), pointer :: dtectptr

    real(eb) :: thickness, surface_temperature, temperature_depth, rti, setpoint, spray_density
    real(eb),dimension(3) :: location,normal
    real(eb),dimension(2) :: setpoints
    character(len=64) :: comp_id, id, matl_id, type, depth_units, surface_orientation
    character(len=128) :: fyi
    logical :: adiabatic_target
    real(eb), dimension(2) :: convection_coefficients
    namelist /DEVC/ comp_id, type, id, temperature_depth, depth_units, location, matl_id, normal, surface_orientation, &
        surface_temperature, thickness, rti, setpoint, spray_density, setpoints, adiabatic_target, convection_coefficients, fyi

    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0

    ! Scan entire file to look for 'DEVC'
    n_targets = 0
    n_detectors= 0
    devc_loop: do
        call checkread ('DEVC',lu,ios)
        if (ios==0) devcflag=.true.
        if (ios==1) then
            exit devc_loop
        end if
        read(lu,DEVC,err=34,iostat=ios)
        if (type == 'PLATE' .or. type == 'CYLINDER') n_targets =n_targets + 1
        if (type == 'SPRINKLER' .or. type == 'HEAT_DETECTOR'.or. type == 'SMOKE_DETECTOR') n_detectors = n_detectors + 1
34      if (ios>0) then
            write(errormessage, '(a,i0)') '***Error in &DEVC: Invalid specification for inputs. Check &DEVC input, ' , &
                n_targets+n_detectors
            call cfastexit('read_devc',1)
        end if
    end do devc_loop

    if (n_targets>mxtarg) then
        write (errormessage,'(a,i0)') '***Error, Too many targets in input data file. Limit is ', mxtarg
        call cfastexit('read_devc',2)
    end if

    if (n_detectors>mxdtect) then
        write (errormessage,'(a,i0)') '***Error, Too many detectors in input data file. Limit is ', mxdtect
        call cfastexit('read_devc',3)
    end if

    devc_detec_flag: if (devcflag) then

        rewind (lu)
        input_file_line_number = 0

        counter1 = 0
        counter2 = 0
        ! Assign value to CFAST variables for further calculations
        read_devc_loop: do ii=1 , n_targets + n_detectors

            call checkread('DEVC',lu,ios)
            call set_defaults
            read(lu,DEVC)

            if (trim(type) == 'PLATE' .or. trim(type) == 'CYLINDER') then
                counter1 = counter1 + 1

                targptr => targetinfo(counter1)

                iroom=0
                compartment_id = 'NULL'
                compartment_id = trim(comp_id)

                idcheck=.false.
                searching: do jj = 1, n_rooms
                    roomptr => roominfo(jj)
                    if (trim(compartment_id) == trim(roomptr%id)) then
                        iroom = roomptr%compartment
                        idcheck = .true.
                        exit searching
                    end if
                end do searching

                if (.not. idcheck) then
                    write (errormessage,'(3a,i0)') '***Error in &DEVC: COMP_ID: ', id, ', not found. Check target, ', counter1
                    call cfastexit('read_devc',4)
                end if

                if (iroom<1.or.iroom>n_rooms+1) then
                    write (errormessage,5003) iroom
                    call cfastexit('read_devc',5)
                end if

                targptr%room = iroom
                targptr%room_id = compartment_id

                ! position and normal vector
                if (convert_negative_distances) then
                    if (location(1)<0._eb) location(1) = roomptr%cwidth + location(1)
                    if (location(2)<0._eb) location(2) = roomptr%cdepth + location(2)
                    if (location(3)<0._eb) location(3) = roomptr%cheight + location(3)
                end if
                targptr%center = location
                targptr%normal = normal
                targptr%surface_orientation = surface_orientation
                targptr%surface_temperature = surface_temperature
                
                targptr%thickness = thickness

                targptr%depth_loc = temperature_depth
                targptr%depth_units = depth_units

                ! target name
                if (.not.newid(id)) then
                    write(errormessage,'(a,a,a,i3)') '***Error, Not a unique identifier for &DEVC ',trim(id), 'Check target ', &
                        counter1
                    call cfastexit('read_devc', 6)
                end if
                targptr%id = id
                
                targptr%fyi = fyi

                ! material type
                tcname = matl_id
                if (tcname=='NULL') tcname = 'DEFAULT'
                targptr%material = tcname
                targptr%wall = 0

                ! equation type, pde or cyl.  ode is outdated and changed to pde if it's in an input file.
                if (type=='PLATE') then
                    targptr%equaton_type = pde
                else if (type=='CYLINDER') then
                    targptr%equaton_type = cylpde
                else
                    write (errormessage,5002) type
                    call cfastexit('read_devc',7)
                end if
                
                ! adiabatic condition
                targptr%adiabatic = .false.
                targptr%adiabatic = adiabatic_target
                
                ! convective heat transfer coefficient
                targptr%h_conv(:) = 0._eb
                targptr%h_conv(1) = convection_coefficients(1)*1000._eb ! W/m^2-K is used during calculation
                targptr%h_conv(2) = convection_coefficients(2)*1000._eb  ! W/m^2-K is used during calculation

            else if (trim(type) == 'SPRINKLER' .or. trim(type) == 'HEAT_DETECTOR'.or. trim(type) == 'SMOKE_DETECTOR') then
                counter2 = counter2 + 1

                dtectptr => detectorinfo(counter2)

                if (trim(type) == 'SMOKE_DETECTOR') then
                    i1 = smoked
                else if (trim(type) == 'HEAT_DETECTOR') then
                    i1 = heatd
                else if (trim(type) == 'SPRINKLER') then
                    i1 = sprinkd
                else
                    write (errormessage,'(a,a)') '***Error in &DEVC: Bad type. Not known for ', type
                    call cfastexit('read_devc',8)
                end if

                dtectptr%dtype = i1

                iroom = 0
                compartment_id = ' '
                compartment_id = trim(comp_id)

                idcheck=.false.
                searching_2: do jj = 1, n_rooms
                    roomptr => roominfo(jj)
                    if (trim(compartment_id) == trim(roomptr%id)) then
                        iroom = roomptr%compartment
                        idcheck = .true.
                        exit searching_2
                    end if
                end do searching_2

                if (.not. idcheck) then
                    write (errormessage,'(3a,i0)') '***Error in &DEVC: COMP_ID: ', id, ', not found. Check device, ', counter2
                    call cfastexit('read_devc',9)
                end if

                dtectptr%room = iroom
                dtectptr%room_id = compartment_id
                
                if (iroom<1.or.iroom>mxrooms) then
                    write (errormessage,5342) iroom
                    call cfastexit('read_devc',10)
                end if

                if (.not.newid(id)) then
                    write(errormessage,'(3a,i0)') '***Error, Not a unique identifier for &DEVC ',trim(id), 'Check detector ', &
                        counter2
                    call cfastexit('read_devc', 11)
                end if
                dtectptr%id = id
                dtectptr%fyi = fyi
                if (trim(type) == 'SPRINKLER' .or. trim(type) == 'HEAT_DETECTOR') then
                    if (setpoint/=-1001._eb) then
                        dtectptr%trigger = setpoint + 273.15_eb
                    else
                        dtectptr%trigger = default_activation_temperature
                    end if
                else
                    if (setpoint/=-1001._eb) then
                        dtectptr%trigger = setpoint
                        dtectptr%dual_detector = .FALSE. 
                    else if (setpoints(1) /= -1001._eb) then
                        dtectptr%trigger = setpoints(2)
                        dtectptr%trigger_smolder = setpoints(1)
                        dtectptr%dual_detector = .TRUE.
                    else
                        dtectptr%trigger = default_activation_obscuration
                        dtectptr%dual_detector = .FALSE. 
                    end if
                end if

                ! position and normal vector
                if (convert_negative_distances) then
                    if (location(1)<0._eb) location(1) = roomptr%cwidth + location(1)
                    if (location(2)<0._eb) location(2) = roomptr%cdepth + location(2)
                    if (location(3)<0._eb) location(3) = roomptr%cheight + location(3)
                end if
                dtectptr%center = location
                
                ! detector response and sprinkler flowrate
                dtectptr%rti =  rti
                if (trim(type) == 'SPRINKLER') then
                    if (rti>0) then
                        dtectptr%quench = .true.
                    else
                        dtectptr%quench = .false.
                    end if
                end if

                dtectptr%spray_density = spray_density*1000.0_eb
                ! if spray density is zero, then turn off the sprinkler
                if (dtectptr%spray_density <= 0.0_eb) then
                    dtectptr%quench = .false.
                end if
                ! if there's a sprinkler that can go off, then make sure the time step is small enough to report it accurately
                if (dtectptr%quench) then
                    if (stpmax>0) then
                        stpmax = min(stpmax,1.0_eb)
                    else
                        stpmax = 1.0_eb
                    end if
                end if

                if (dtectptr%center(1)>roomptr%cwidth.or. &
                    dtectptr%center(2)>roomptr%cdepth.or.dtectptr%center(3)>roomptr%cheight) then
                    write (errormessage,5339) n_detectors,roomptr%id
                    call cfastexit('read_devc',12)
                end if

            end if

        end do read_devc_loop

    end if devc_detec_flag

5002 format ('***Error, Bad DEVC input. Invalid equation type: ',a,' Valid choices are: PDE or CYL')
5003 format ('***Error, Bad DEVC input. The compartment specified by DEVC does not exist ',i0)

5339 format ('***Error, Bad DEVC input. Device ',i0,' is outside of compartment ',a)
5342 format ('***Error, Bad DEVC input. Invalid compartment specification ',i0)



    contains

    subroutine set_defaults

    comp_id                         = 'NULL'
    type                            = 'NULL'
    fyi                             = 'NULL'
    id                              = 'NULL'
    temperature_depth               = 0.5_eb
    depth_units                     = 'FRACTION'
    location(:)                     = (/-1.0_eb, -1.0_eb, -3.0_eb/39.37_eb/)
    matl_id                         = 'NULL'
    normal(:)                       = (/0., 0., 1./)
    surface_orientation             = "NULL"
    surface_temperature             = interior_ambient_temperature
    thickness                       = 0._eb
    rti                             = default_rti
    setpoint                        = -1001._eb
    setpoints                       = (/-1001._eb, -1001._eb/)
    spray_density                   = -300.0_eb
    adiabatic_target                = .false.
    convection_coefficients(:)      = 0._eb

    end subroutine set_defaults

    end subroutine read_devc


    ! --------------------------- read_ramp -------------------------------------------
    
!> \brief   read in &RAMP namelist that includes time ramp specifications (deprecated at this point)
    
!> \param   lu (input): logical input unit number for the open input file
    
    subroutine read_ramp (lu)

    integer, intent(in) :: lu
    
    integer :: ii, ios

    type(ramp_type), pointer :: rampptr

    real(eb), dimension(mxpts) :: f, t, z
    character(len=64) :: type,id
    character(len=64), dimension(2) :: comp_ids
    namelist /RAMP/ f, id ,t, z, type, comp_ids

    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0

    ! Scan entire file to look for 'RAMP'
    n_ramps = 0
    ramp_loop: do
        call checkread ('RAMP',lu,ios)
        if (ios==0) rampflag=.true.
        if (ios==1) then
            exit ramp_loop
        end if
        read(lu,RAMP,iostat=ios)
        n_ramps =n_ramps + 1
        if (ios>0) then
            write(errormessage, '(a,i0)') '***Error in &RAMP: Invalid specification for inputs. Check &RAMP input, ', n_ramps
            call cfastexit('read_ramp',1)
        end if
    end do ramp_loop

    if (n_ramps>mxramps) then
        write (errormessage,'(a,i0)') '***Error, Too many ramps in input data file. Limit is ', mxramps
        call cfastexit('read_ramp',2)
    end if

    ramp_flag: if (rampflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_ramp_loop: do ii = 1,n_ramps

            call checkread('RAMP',lu,ios)
            call set_defaults
            read(lu,RAMP)

            rampptr => rampinfo(ii)
            rampptr%id = id
            if (count(z/=-1001._eb)>0 .and. count(t/=-1001._eb)>0) then
                write (errormessage,'(a,i0)') '***Error in &RAMP: Cannot use both z and t in a ramp. Check ramp, ', n_ramps
                call cfastexit('read_ramp',3)
            else if (count(z/=-1001._eb)==0 .and. count(t/=-1001._eb)==0) then
                write (errormessage,'(a,i0)') '***Error in &RAMP: Either z or t must be in a ramp. Check ramp, ', n_ramps
                call cfastexit('read_ramp',4)
            end if
            
            if (type=='AREA' .and. count(z/=-1001._eb)>0) then
                rampptr%x(1:mxpts)  = z(1:mxpts)
            else
                rampptr%x(1:mxpts) = t(1:mxpts)
            end if
            rampptr%f_of_x(1:mxpts) = f(1:mxpts)

            if (count(rampptr%x/=-1001._eb) /= count(rampptr%f_of_x/=-1001._eb)) then
                if (type=='AREA') then
                    write (errormessage,'(a,i0)') &
                        '***Error in &RAMP: The number of inputs for z and f do not match. Check ramp, ', n_ramps
                    call cfastexit('read_ramp',5)
                else
                    write (errormessage,'(a,i0)') &
                        '***Error in &RAMP: The number of inputs for t and f do not match. Check ramp, ', n_ramps
                    call cfastexit('read_ramp',6)
                end if
            end if
            rampptr%npoints=count(rampptr%x/=-1001._eb)

        end do read_ramp_loop

    end if ramp_flag


    contains

    subroutine set_defaults

    type                    = 'NULL'
    t(:)                    = -1001._eb
    f(:)                    = -1001._eb
    z(:)                    = -1001._eb
    id                      = 'NULL'

    end subroutine set_defaults

    end subroutine read_ramp


    ! --------------------------- read_tabl ------------------------
    
!> \brief   read in &TABL namelist that includes fire time history inputs
    
!> \param   lu (input): logical input unit number for the open input file
    
    subroutine read_tabl (lu)
    
    integer, intent(in) :: lu
    
    integer :: ios, i, ii, jj, n_tabl_lines

    type(table_type),   pointer :: tablptr

    character(len=64) :: id
    character(len=64), dimension(mxtablcols) :: labels
    real(eb), dimension(mxtablcols) :: data
    
    namelist /TABL/ id, labels, data

    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0
    n_tabl_lines = 0

    ! Scan entire file to look for 'TABL' and identify unique table names
    n_tabls = 0
    search_loop: do
        call checkread('TABL',lu,ios)
        if (ios==0) tablflag = .true.
        if (ios==1) exit search_loop
        read(lu,tabl,iostat=ios)
        if (ios>0) then
            write(errormessage, '(a,i0)') '***Error in &TABL: Invalid specification for inputs. Check &TABL input, ', n_tabls+1
            call cfastexit('read_tabl',1)
        end if
        do i = 1, n_tabls
            tablptr => tablinfo(i)
            if(id==tablptr%id) then
                n_tabl_lines = n_tabl_lines +1
                cycle search_loop
            end if
        end do
        n_tabls = n_tabls + 1
        if (n_tabls>mxtabls) then
            write (errormessage,'(a,i0)') '***Error, Too many tables in input data file. Limit is ', mxfires
            call cfastexit('read_tabl',2)
        end if
        n_tabl_lines = n_tabl_lines +1
        tablptr => tablinfo(n_tabls)
        tablptr%id = id
        tablptr%n_points = 0

    enddo search_loop

    tabl_flag: if (tablflag) then

        ! gather column names and data for use later on
        read_tabl_loop: do ii = 1, n_tabls
            tablptr => tablinfo(ii)
            rewind (lu)
            input_file_line_number = 0
            do jj = 1, n_tabl_lines
                call checkread('TABL',lu,ios)
                call set_defaults
                read(lu,TABL)
                if (id==tablptr%id) then
                    if(labels(1)/='NULL') then
                        ! input is column headings
                        tablptr%n_columns = 0
                        do i = 1,mxtablcols
                            if (labels(i)/='NULL') then
                                tablptr%labels(i) = labels(i)
                                tablptr%n_columns = tablptr%n_columns + 1
                            end if
                        end do
                    else
                        ! input is a row of data for the table
                        if (data(1)/=-1001._eb) then
                            tablptr%n_points = tablptr%n_points +1
                            do i = 1,mxtablcols
                                if (data(i)/=-1001._eb) then
                                    tablptr%data(tablptr%n_points,i) = data(i)
                                end if
                            end do
                        end if
                    end if
                end if
            end do
        end do read_tabl_loop
continue
    end if tabl_flag

    contains

    subroutine set_defaults

    id                    = 'NULL'
    labels(:)             = 'NULL'
    data(:)               = -1001._eb

    end subroutine set_defaults
    
    end subroutine read_tabl

    ! --------------------------- read_fire ----------------------------------
    
!> \brief   read in &FIRE namelist that includes fire specifications
    
!> \param   lu (input): logical input unit number for the open input file
    
    subroutine read_fire (lu)

    integer, intent(in) :: lu
    
    integer :: ios, i, ii, jj, iroom
    real(eb) :: tmpcond
    character(len=64) :: compartment_id

    type(room_type),   pointer :: roomptr
    type(fire_type),   pointer :: fireptr
    type(target_type), pointer :: targptr

    real(eb) setpoint
    character(len=64) :: comp_id, devc_id, fire_id, id, ignition_criterion
    character(len=128) :: fyi
    real(eb), dimension(2) :: location
    
    namelist /FIRE/ comp_id, devc_id, fire_id, id, ignition_criterion, location, setpoint, fyi

    ios = 1
    tmpcond = 0.0

    rewind (unit=lu)
    input_file_line_number = 0

    ! Scan entire file to look for 'FIRE'
    n_fires = 0
    insf_loop: do
        call checkread ('FIRE', lu, ios)
        if (ios==0) insfflag = .true.
        if (ios==1) then
            exit insf_loop
        end if
        read(lu,FIRE,iostat=ios)
        if (ios>0) then
            write(errormessage, '(a,i0)') '***Error in &FIRE: Invalid specification for inputs. Check &FIRE input, ', n_fires+1
            call cfastexit('read_fire',1)
        end if
        n_fires =n_fires + 1
    end do insf_loop

    if (n_fires>mxfires) then
        write (errormessage,'(a,i3)') '***Error, Too many fires in input data file. Limit is ', mxfires
        call cfastexit('read_fire',2)
    end if

    insf_flag: if (insfflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign values to CFAST variables for further calculations
        read_insf_loop: do ii = 1, n_fires

            fireptr => fireinfo(ii)

            call checkread('FIRE',lu,ios)
            call set_defaults
            read(lu,FIRE)

            iroom = 0
            compartment_id = ' '
            compartment_id = trim(comp_id)

            searching: do jj = 1, n_rooms
                roomptr => roominfo(jj)
                if (trim(compartment_id) == trim(roomptr%id)) then
                    iroom = roomptr%compartment
                    exit searching
                end if
            end do searching

            if (iroom<1.or.iroom>n_rooms) then
                write (errormessage,5320) iroom
                call cfastexit('read_fire',3)
            end if
            roomptr => roominfo(iroom)

            fireptr%room = iroom
            if (.not.newid(id)) then
                write(errormessage,'(a,a,a,i3)') '****Error, Not a unique identifier for &DEVC ',trim(id), 'Check target ',ii
                call cfastexit('read_fire', 4)
            end if
            fireptr%id = id
            fireptr%fyi = fyi
            fireptr%fire_id = fire_id
            
            ! position
            if (convert_negative_distances) then
                if (location(1)<0._eb) location(1) = roomptr%cwidth + location(1)
                if (location(2)<0._eb) location(2) = roomptr%cdepth + location(2)
            end if
            fireptr%x_position = location(1)
            fireptr%y_position = location(2)
            fireptr%z_position = 0.0_eb
            if (fireptr%x_position>roomptr%cwidth.or.fireptr%y_position>roomptr%cdepth.or.fireptr%z_position>roomptr%cheight) then
                write (errormessage,5323) ii
                call cfastexit('read_fire',5)
            end if

            if (trim(ignition_criterion) /= 'NULL') then
                if (trim(ignition_criterion)=='TIME' .or. trim(ignition_criterion)=='TEMPERATURE' .or. &
                    trim(ignition_criterion)=='FLUX') then
                    ! it's a new format fire line that point to an existing target rather than to one created for the fire
                    if (trim(ignition_criterion)=='TIME') fireptr%ignition_type = trigger_by_time
                    if (trim(ignition_criterion)=='TEMPERATURE') fireptr%ignition_type = trigger_by_temp
                    if (trim(ignition_criterion)=='FLUX') fireptr%ignition_type = trigger_by_flux
                    tmpcond = setpoint
                    fireptr%ignition_target = 0
                    if (trim(ignition_criterion)=='TEMPERATURE' .or. trim(ignition_criterion)=='FLUX') then
                        do i = 1,n_targets
                            targptr => targetinfo(i)
                            if (trim(targptr%id)==trim(devc_id)) fireptr%ignition_target = i
                        end do
                        if (fireptr%ignition_target==0) then
                            write (errormessage,5324) n_fires
                            call cfastexit('read_fire',6)
                        end if
                    end if
                else
                    write (errormessage,5322)
                    call cfastexit('read_fire',7)
                end if
            end if

            ! note that ignition type 1 is time, type 2 is temperature and 3 is flux
            if (tmpcond>0.0_eb) then
                fireptr%ignited = .false.
                if (fireptr%ignition_type==trigger_by_time) then
                    fireptr%ignition_time = tmpcond
                    fireptr%ignition_criterion = 1.0e30_eb !check units
                else if (fireptr%ignition_type==trigger_by_temp) then
                    fireptr%ignition_time = 1.0e30_eb  !check units
                    fireptr%ignition_criterion = tmpcond + kelvin_c_offset
                    if (stpmax>0) then
                        stpmax = min(stpmax,1.0_eb)
                    else
                        stpmax = 1.0_eb
                    end if
                else if (fireptr%ignition_type==trigger_by_flux) then
                    fireptr%ignition_time = 1.0e30_eb  !check units
                    fireptr%ignition_criterion = tmpcond * 1000._eb
                    if (stpmax>0) then
                        stpmax = min(stpmax,1.0_eb)
                    else
                        stpmax = 1.0_eb
                    end if
                else
                    write (errormessage,5358) fireptr%ignition_type
                    call cfastexit('read_fire',8)
                end if
            else
                fireptr%ignited  = .true.
                fireptr%reported = .true.
            end if

        end do read_insf_loop

    end if insf_flag

5320 format ('***Error, Bad FIRE input. Fire specification error, compartment ',i0,' out of range')
5321 format ('***Error, Bad FIRE input. Fire specification error, not an allowed fire type',i0)
5322 format ('***Error, Bad FIRE input. Fire specification is outdated and must include target for ignition')
5323 format ('***Error, Bad FIRE input. Fire location ',i0,' is outside its compartment')
5324 format ('***Error, Bad FIRE input. Target specified for fire ',i0, ' does not exist')
5358 format ('***Error, Bad FIRE input. Not a valid ignition criterion ',i0)

5001 format ('***Error, invalid heat of combustion, must be greater than zero, ',1pg12.3)
5002 format ('***Error, invalid fire area. all input values must be greater than zero')
5106 format ('***Error, object ',a,' position set to ',3f7.3,'; maximum hrr per m^3 = ',1pg10.3,' exceeds physical limits')
5107 format ('Object ',a,' position set to ',3f7.3,'; maximum c_hrr per m^3 = ',1pg10.3,' exceeds nominal limits')
5000 format ('***Error, the key word ',a5,' is not part of a fire definition. fire keywords are likely out of order')

    contains

    subroutine set_defaults

    comp_id                 = 'NULL'
    devc_id                 = 'NULL'
    fire_id                 = 'NULL'
    id                      = 'NULL'
    fyi                     = 'NULL'
    ignition_criterion      = 'TIME'
    location(:)             = 0._eb
    setpoint                  = 0._eb

    end subroutine set_defaults
    
    end subroutine read_fire


    ! --------------------------- read_chem -------------------------------------------
    
!> \brief   read in &CHEM namelist that includes fire chemsitry specifications
    
!> \param   lu (input): logical input unit number for the open input file
    
    subroutine read_chem (lu)

    integer, intent(in) :: lu
    
    integer :: ios, i, ii, jj, kk, n_defs, ifire !, np
    real(eb) :: tmpcond, max_hrr, f_height, hrrpm3, max_area, ohcomb

    type(fire_type),   pointer :: fireptr
    type(table_type),   pointer :: tablptr

    real(eb) :: carbon, chlorine, hydrogen, nitrogen, oxygen
    real(eb) :: area, co_yield, hcl_yield, hcn_yield, heat_of_combustion, hrr, radiative_fraction, &
        soot_yield, trace_yield, flaming_transition_time
    character(len=64) :: comp_id, id, table_id
    namelist /CHEM/ area, carbon, chlorine, comp_id, co_yield, heat_of_combustion, &
        hcl_yield, hcn_yield, hrr, hydrogen, id, nitrogen, oxygen, radiative_fraction, soot_yield, &
        table_id, trace_yield, flaming_transition_time

    ios = 1
    tmpcond = 0.0

    rewind (unit=lu)
    input_file_line_number = 0

    ! Scan entire file to look for 'FIRE'
    n_defs = 0
    fire_loop: do
        call checkread ('CHEM', lu, ios)
        if (ios==0) fireflag = .true.
        if (ios==1) then
            exit fire_loop
        end if
        read(lu,CHEM,iostat=ios)
        if (ios>0) then
            write(errormessage, '(a,i0)') '***Error in &CHEM: Invalid specification for inputs. Check &CHEM input, ', n_defs+1
            call cfastexit('read_chem',1)
        end if
        n_defs =n_defs + 1
    end do fire_loop

    if (n_defs>mxfires) then
        write (errormessage,'(a,i0)') '***Error, Too many fires in input data file. Limit is ', mxfires
        call cfastexit('read_chem',2)
    end if

    fire_flag: if (fireflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations.
        !This just adds information to previously read and defined fires from &FIRE
        read_fire_loop: do ii = 1, n_defs

            call checkread('CHEM',lu,ios)
            call set_defaults
            read(lu,CHEM)

            ifire = 0
            
            ! find all fires that this definition applies to
            searching: do jj = 1, n_fires
                fireptr => fireinfo(jj)
                if (trim(id) == trim(fireptr%fire_id)) then
                    ifire =jj


                    fireptr => fireinfo(ifire)
                    fireptr%qdot = 0.0_eb
                    fireptr%y_soot = 0.0_eb
                    fireptr%y_co = 0.0_eb
                    fireptr%y_trace = 0.0_eb
                    fireptr%area = pio4*0.2_eb**2
                    fireptr%height = 0.0_eb

                    ! Only constrained fires
                    fireptr%chemistry_type = 2
                    if (fireptr%chemistry_type>2) then
                        write (errormessage,5321) fireptr%chemistry_type
                        call cfastexit('read_chem',3)
                    end if

                    ! Define chemical formula
                    fireptr%n_c  = carbon
                    fireptr%n_h  = hydrogen
                    fireptr%n_o  = oxygen
                    fireptr%n_n  = nitrogen
                    fireptr%n_cl = chlorine
                    fireptr%molar_mass = (12.01_eb*fireptr%n_c + 1.008_eb*fireptr%n_h + 16.0_eb*fireptr%n_o + &
                        14.01_eb*fireptr%n_n + 35.45_eb*fireptr%n_cl)/1000.0_eb
                    fireptr%chirad = radiative_fraction
                    fireptr%flaming_transition_time = flaming_transition_time
                    ohcomb = heat_of_combustion *1.e3_eb
                    if (ohcomb<=0.0_eb) then
                        write (errormessage,5001) ohcomb
                        call cfastexit('read_chem',4)
                    end if

                    ! do constant values for fire inputs first, then check for time-varying inputs

                    ! constant hrr
                    fireptr%n_qdot = 1
                    fireptr%t_qdot(1) = 0.0_eb
                    fireptr%qdot(1) = hrr * 1000._eb
                    max_hrr = hrr

                    ! constant soot
                    fireptr%n_soot = 1
                    fireptr%t_soot(1) = 0.0_eb
                    fireptr%y_soot(1) = soot_yield

                    ! constant co
                    fireptr%n_co = 1
                    fireptr%t_co(1) = 0.0_eb
                    fireptr%y_co(1) = co_yield

                    ! constant hcn
                    fireptr%n_hcn = 1
                    fireptr%t_hcn(1) = 0.0_eb
                    fireptr%y_hcn(1) = hcn_yield

                    ! constant trace species
                    fireptr%n_trace = 1
                    fireptr%t_trace(1) = 0.0_eb
                    fireptr%y_trace(1) = trace_yield

                    ! constant area
                    fireptr%n_area = 1
                    fireptr%t_area(1) = 0.0_eb
                    fireptr%area(1) = max(area,pio4*0.2_eb**2)

                    ! constant height
                    fireptr%n_height = 1
                    fireptr%t_height = 0.0_eb
                    fireptr%height(1) = 0.0_eb

                    tabl_search: do kk = 1, n_tabls
                        tablptr=>tablinfo(kk)
                        if (trim(tablptr%id)==trim(fireptr%fire_id)) then
                            call fireptr%pop_table(tablptr)
                            !np = tablptr%n_points
                            !do i = 1,mxtablcols
                            !    select case (trim(tablptr%labels(i)))
                            !    case ('TIME')
                            !        fireptr%t_qdot(1:np) = tablptr%data(1:np,i)
                            !        fireptr%n_qdot = np
                            !        fireptr%t_soot(1:np) = tablptr%data(1:np,i)
                            !        fireptr%n_soot = np
                            !        fireptr%t_co(1:np) = tablptr%data(1:np,i)
                            !        fireptr%n_co = np
                            !        fireptr%t_hcn(1:np) = tablptr%data(1:np,i)
                            !        fireptr%n_hcn = np
                            !        fireptr%t_trace(1:np) = tablptr%data(1:np,i)
                            !        fireptr%n_trace = np
                            !        fireptr%t_area(1:np) = tablptr%data(1:np,i)
                            !        fireptr%n_area = np
                            !        fireptr%t_height(1:np) = tablptr%data(1:np,i)
                            !        fireptr%n_height = np
                            !    case ('HRR')
                            !        fireptr%qdot(1:np) = tablptr%data(1:np,i)*1000._eb
                            !    case ('HEIGHT')
                            !        fireptr%height(1:np) = tablptr%data(1:np,i)
                            !    case ('AREA')
                            !        fireptr%area(1:np) = max(tablptr%data(1:np,i),pio4*0.2_eb**2)
                            !    case ('CO_YIELD')
                            !        fireptr%y_co(1:np) = tablptr%data(1:np,i)
                            !    case ('SOOT_YIELD')
                            !        fireptr%y_soot(1:np) = tablptr%data(1:np,i)
                            !    case ('HCN_YIELD')
                            !        fireptr%y_hcn(1:np) = tablptr%data(1:np,i)
                            !    case ('HCL_YIELD')
                            !        ! with nothing here, all chlorine in the fuel is assumed to go to HCl.
                            !    case ('TRACE_YIELD')
                            !        fireptr%y_trace(1:np) = tablptr%data(1:np,i)
                            !    end select
                            !end do
                        end if
                    end do tabl_search

                    ! calculate mass loss rate from hrr and hoc inputs
                    fireptr%mdot = fireptr%qdot / ohcomb
                    fireptr%t_mdot = fireptr%t_qdot
                    fireptr%n_mdot = fireptr%n_qdot
                    ! set the heat of combustion - this is a problem if the qdot is zero and the mdot is zero as well
                    call set_heat_of_combustion (fireptr%n_qdot, ohcomb, fireptr%mdot, fireptr%qdot, fireptr%hoc)
                    fireptr%t_hoc = fireptr%t_qdot
                    fireptr%n_hoc = fireptr%n_qdot

                    ! maximum area, used for input check of hrr per flame volume
                    max_area = 0.0_eb
                    do i = 1, fireptr%n_area
                        max_area = max(max_area,max(fireptr%area(i),pio4*0.2_eb**2))
                    end do
                    if (max_area==0.0_eb) then
                        write (errormessage,5002)
                        write (iofill,5002)
                    end if
                    fireptr%firearea = max_area

                    ! calculate a characteristic length of an object (we assume the diameter).
                    ! this is used for point source radiation fire to target calculation as a minimum effective
                    ! distance between the fire and the target which only impacts very small fire to target distances
                    fireptr%characteristic_length = sqrt(max_area/pio4)

                    ! Diagnostic - check for the maximum heat release per unit volume.
                    ! First, estimate the flame length - we want to get an idea of the size of the volume over which the energy will be released
                    f_height = flame_height(max_hrr, max_area)
                    f_height = max (0.0_eb, f_height)

                    ! Now the heat release per cubic meter of the flame - we know that the size is larger than 1.0d-6 m^3 - enforced above
                    hrrpm3 = max_hrr/(pio4*fireptr%characteristic_length**2*(fireptr%characteristic_length+f_height))
                    if (hrrpm3>4.0e6_eb) then
                        write (errormessage,5106) trim(fireptr%id),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
                        call cfastexit('read_chem',6)
                    end if
                end if
            end do searching

            if (ifire<1.or.ifire>n_fires) then
                write (errormessage,5320) ifire
                call cfastexit('read_chem',7)
            end if

        end do read_fire_loop

    end if fire_flag

5320 format ('***Error, Bad FIRE input. Fire specification error, fire ',i0,' is not referenced')
5321 format ('***Error, Bad FIRE input. Fire specification error, not an allowed fire type',i0)
5322 format ('***Error, Bad FIRE input. Fire specification is outdated and must include target for ignition')
5323 format ('***Error, Bad FIRE input. Fire location ',i0,' is outside its compartment')
5324 format ('***Error, Bad FIRE input. Target specified for fire ',i0, ' does not exist')
5358 format ('***Error, Bad FIRE input. Not a valid ignition criterion ',i0)

5001 format ('***Error, invalid heat of combustion, must be greater than zero, ',1pg12.3)
5002 format ('***Error, invalid fire area. all input values must be greater than zero')
5106 format ('***Error, object ',a,' position set to ',3f7.3,'; maximum hrr per m^3 = ',1pg10.3,' exceeds physical limits')
5107 format ('Object ',a,' position set to ',3f7.3,'; maximum c_hrr per m^3 = ',1pg10.3,' exceeds nominal limits')
5000 format ('***Error, the key word ',a,' is not part of a fire definition. fire keywords are likely out of order')

    contains

    subroutine set_defaults

    area                      = 0._eb
    carbon                    = 0._eb
    chlorine                  = 0._eb
    comp_id                   = 'NULL'
    co_yield                  = 0._eb
    hcn_yield                 = 0.0_eb
    heat_of_combustion        = 50000._eb
    hrr                       = 0.0_eb
    hydrogen                  = 0._eb
    id                        = 'NULL'
    nitrogen                  = 0._eb
    oxygen                    = 0._eb
    radiative_fraction        = 0._eb
    soot_yield                = 0._eb
    table_id                  = 'NULL'
    trace_yield               = 0._eb
    flaming_transition_time   = 0._eb

    end subroutine set_defaults

    ! --------------------------- set_heat_of_combustion -------------------------------------------
    
!> \brief   calculates a (typically) constant or time-dependent heat of combustion
    
!> \param   maxint (input): number of data points in the neat of combustion curve; typically 1
!> \param   hinitial (input): constant heat of combustion
!> \param   qdot (input): heat release rate (kW)
!> \param   hdot (output): time-dependent heat of combustion (kJ/kg)
!> \param   mdot (output): mass loss rate (kg/s)

    subroutine set_heat_of_combustion (maxint, hinitial, mdot, qdot, hdot)

    ! set the heat of combustion for all fires

    integer, intent(in) :: maxint
    real(eb), intent(in) :: qdot(maxint), hinitial
    real(eb), intent(out) :: mdot(maxint), hdot(maxint)

    integer :: i
    real(eb) :: hcmax = 1.0e8_eb, hcmin = 1.0e6_eb

    do i = 1, maxint
        if (i>1) then
            if (mdot(i)*qdot(i)<=0.0_eb) then
                hdot(i) = hinitial
            else
                hdot(i) = min(hcmax,max(qdot(i)/mdot(i),hcmin))
                mdot(i) = qdot(i)/hdot(i)
            end if
        else
            hdot(1) = hinitial
        end if
    end do

    return

    end subroutine set_heat_of_combustion

    end subroutine read_chem


    ! --------------------------- read_vent -------------------------------------------
    
!> \brief   read in &VENT namelist that includes all vent specifications
    
!> \param   lu (input): logical input unit number for the open input file

    subroutine read_vent (lu)

    integer, intent(in) :: lu

    integer :: i, ii, j, jj, k, mm, imin, jmax, counter1, counter2, counter3, iroom, ipts, ic
    integer :: ios
    character(len=64) :: compartment_id

    type(room_type), pointer :: roomptr
    type(target_type), pointer :: targptr
    type(vent_type), pointer :: ventptr

    real(eb) :: area, bottom, flow, height, offset, setpoint, top, width, pre_fraction, post_fraction, &
        filter_time, filter_efficiency
    real(eb), dimension(2) :: areas, cutoffs, heights, offsets
    real(eb), dimension(mxpts) :: t, f
    character(len=64),dimension(2) :: comp_ids, orientations
    character(len=64) :: criterion, devc_id, face, id, shape, type
    character(len=128) :: fyi
    namelist /VENT/ area, areas, bottom, comp_ids, criterion, cutoffs, devc_id, f, face, filter_efficiency, &
        filter_time, flow, height, heights, id, offset, offsets, orientations, pre_fraction, post_fraction, &
        setpoint, shape, t, top, type, width, fyi

    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0

    ! Scan entire file to look for 'VENT'
    n_hvents = 0
    n_mvents = 0
    n_vvents = 0
    vent_loop: do
        call checkread ('VENT',lu,ios)
        if (ios==0) ventflag=.true.
        if (ios==1) then
            exit vent_loop
        end if
        read(lu,VENT,err=34,iostat=ios)
        if (trim(type) == 'WALL') n_hvents =n_hvents + 1
        if (trim(type) == 'MECHANICAL') n_mvents =n_mvents + 1
        if (trim(type) == 'CEILING' .or. trim(type) == 'FLOOR') n_vvents =n_vvents + 1
34      if (ios>0) then
            write(errormessage, '(3a,i0)') '***Error in &VENT: Invalid specification for inputs. Check &VENT input, ',trim(id), &
                ': ', n_hvents + n_mvents + n_vvents
            call cfastexit('read_vent',1)
        end if
    end do vent_loop

    if (n_hvents>mxhvents) then
        write (errormessage,'(a,i0)') '***Error, Too many wall vents in input data file. Limit is ', mxhvents
        call cfastexit('read_vent',2)
    end if

    if (n_mvents>mxmvents) then
        write (errormessage,'(a,i0)') '***Error, Too many mechanical vents in input data file. Limit is ', mxmvents
        call cfastexit('read_vent',3)
    end if

    if (n_vvents>mxvvents) then
        write (errormessage,'(a,i0)') '***Error, Too many celing/floor vents in input data file. Limit is ', mxvvents
        call cfastexit('read_vent',4)
    end if

    vent_flag: if (ventflag) then

        rewind (lu)
        input_file_line_number = 0

        counter1=0
        counter2=0
        counter3=0

        ! Assign value to CFAST variables for further calculations
        read_vent_loop: do ii=1,n_hvents+n_mvents+n_vvents

            call checkread('VENT',lu,ios)
            call set_defaults
            read(lu,VENT)

            ! Wall vent
            if (trim(type) == 'WALL') then
                counter1=counter1+1
                
                call find_comp_idxes

                imin = min(i,j)
                jmax = max(i,j)
                
                if (imin>mxrooms-1.or.jmax>mxrooms.or.imin==jmax) then
                    write (errormessage,5070) i, j
                    call cfastexit('read_vent',5)
                end if

                ventptr => hventinfo(counter1)

                if (.not.newid(id)) then
                    write(errormessage,'(3a,i0,a,i0)') '***Error, Not a unique identifier for &VENT ',trim(id), &
                        'Check wall v ', counter1, ' for vent ',trim(id), ' &VENT number ', &
                        counter1 + counter2 + counter3
                    call cfastexit('read_vent', 6)
                end if
                ventptr%id = id
                ventptr%fyi = fyi
                ventptr%room1 = imin
                ventptr%room2 = jmax
                ventptr%counter = counter1

                ! absolute positions are always relative to the floor of the "inside" room
                if (imin == n_rooms+1) then
                    roomptr => roominfo(jmax)
                else
                    roomptr => roominfo(imin)
                end if
                ventptr%absolute_soffit = ventptr%soffit + roomptr%z0
                ventptr%absolute_sill = ventptr%sill + roomptr%z0


                if (n_hvents>mxhvents) then
                    write (errormessage,5081) i, j, k
                    call cfastexit('read_vent',7)
                end if
                
                ! position
                ventptr%width  = width
                if (convert_negative_distances) then
                    roomptr => roominfo(ventptr%room1)
                    if (top<0._eb) top = roomptr%cheight + top
                    if (bottom<0._eb) bottom = roomptr%cheight + bottom
                end if
                ventptr%sill   = bottom
                ventptr%soffit = top
                if (top==0._eb .and. height /= 0._eb) ventptr%soffit = bottom + height

                if (trim(face) == 'FRONT') ventptr%face=1
                if (trim(face) == 'RIGHT') ventptr%face=2
                if (trim(face) == 'REAR') ventptr%face=3
                if (trim(face) == 'LEFT') ventptr%face=4
                
                ventptr%offset(1) = offset
                ventptr%offset(2) = 0._eb
                
                call set_criterion

                ! Mechanical vent
            else if (trim(type) == 'MECHANICAL') then
                counter2=counter2+1

                call find_comp_idxes

                k = counter2
                if (i>n_rooms+1.or.j>n_rooms+1) then
                    write (errormessage,5191) i, j
                    call cfastexit('read_vent',8)
                end if

                ventptr => mventinfo(counter2)

                if (.not.newid(id)) then
                    write(errormessage,'(3a,i0,a,i0)') '***Error, Not a unique identifier for &VENT ',trim(id), &
                        ' Check mech vent ', counter2, ' for vent ',trim(id), ' &VENT number ', &
                        counter1 + counter2 + counter3
                    call cfastexit('read_vent', 6)
                end if
                ventptr%id = id
                ventptr%fyi = fyi
                ventptr%room1 = i
                ventptr%room2 = j
                ventptr%counter = counter2
                ventptr%filter_initial_time = filter_time
                ventptr%filter_final_time = filter_time + 1.0_eb
                ventptr%filter_final_fraction = filter_efficiency / 100.0_eb

                do jj = 1, 2
                    if (orientations(jj) == 'VERTICAL') then
                        ventptr%orientation(jj) = 1
                    else if (orientations(jj) == 'HORIZONTAL') then
                        ventptr%orientation(jj) = 2
                    end if 
                end do 

                ! diffuser locations
                if (convert_negative_distances) then
                    if (ventptr%room1<n_rooms) then
                        roomptr => roominfo(ventptr%room1)
                        if (heights(1)<0._eb) heights(1) = roomptr%cheight + heights(1)
                    end if
                    if (ventptr%room2<n_rooms) then
                        roomptr => roominfo(ventptr%room2)
                        if (heights(2)<0._eb) heights(2) = roomptr%cheight + heights(2)
                    end if
                end if
                ventptr%height(1) = heights(1)
                ventptr%diffuser_area(1) = areas(1)
                ventptr%height(2) = heights(2)
                ventptr%diffuser_area(2) = areas(2)

                ventptr%n_coeffs = 1
                ventptr%coeff = 0.0_eb
                ventptr%coeff(1) = flow
                ventptr%maxflow = flow
                ventptr%min_cutoff_relp = cutoffs(1)
                ventptr%max_cutoff_relp = cutoffs(2)

                ventptr%xoffset = offsets(1)
                ventptr%yoffset = offsets(2)
                
                if (trim(criterion) /= 'NULL') then
                    call set_criterion
                end if

                ! Ceiling/Floor vents
            else if (trim(type) == 'CEILING' .or. trim(type) == 'FLOOR') then
                counter3=counter3+1

                call find_comp_idxes

                k = counter3

                ! check for outside of compartment space; self pointers are covered in read_input_file
                if (i>mxrooms.or.j>mxrooms) then
                    write (errormessage,5070) i, j
                    call cfastexit('read_vent',10)
                end if

                ventptr => vventinfo(counter3)

                if (.not.newid(id)) then
                    write(errormessage,'(3a,i0,a,i0)') '***Error, Not a unique identifier for &VENT ',trim(id), &
                        'Check ceil vent ', counter3, ' for vent ',trim(id), ' &VENT number ', &
                        counter1 + counter2 + counter3
                    call cfastexit('read_vent', 6)
                end if
                ventptr%id = id
                ventptr%fyi = fyi
                ventptr%room1 = i
                ventptr%room2 = j
                ventptr%counter = counter3

                ! read_input_file will verify the orientation (i is on top of j)
                ventptr%area = area

                ! check the shape parameter. the default (1) is a circle)
                if (trim(shape) == 'ROUND') then
                    ventptr%shape = 1
                else if (trim(shape) == 'SQUARE') then
                    ventptr%shape = 2
                else
                    write (errormessage,'(3a,i0)') '***Error, SHAPE must be SQUARE or ROUND. ', shape, ' for vent ', &
                        trim(id), ' &VENT number ', counter1 + counter2 + counter3
                    call cfastexit('read_vent',12)
                end if

                ventptr%xoffset = offsets(1)
                ventptr%yoffset = offsets(2)
                
                if (trim(criterion) /='NULL') then
                    call set_criterion 
                end if
            end if

        end do read_vent_loop

    end if vent_flag

5070 format ('***Error, Bad VENT input. Parameter(s) outside of allowable range',2I4)
5080 format ('***Error, Bad VENT input. Too many pairwise horizontal connections',3I5)
5081 format ('***Error, Too many horizontal connections ',3i5)

5191 format ('***Error, Bad MVENT input. Compartments specified in MVENT have not been defined ',2i3)

    contains

    subroutine set_defaults

    area                  = 0._eb
    areas(:)              = 0._eb
    bottom                = 0._eb
    comp_ids(:)           = 'NULL'
    criterion             = 'TIME'
    cutoffs(1)            = default_min_cutoff_relp
    cutoffs(2)            = default_max_cutoff_relp
    devc_id               = 'NULL'
    f(:)                  = -1001._eb
    face                  = 'NULL'
    filter_time           = 0._eb
    filter_efficiency     = 0._eb
    flow                  = 0._eb
    fyi                   = 'NULL'
    heights(:)            = 0._eb
    id                    = 'NULL'
    offset                = 0._eb
    offsets(:)            = 0._eb
    orientations(:)       = 'VERTICAL'
    pre_fraction          = 1._eb
    post_fraction         = 1._eb
    setpoint              = 0._eb
    shape                 = 'NULL'
    t(:)                  = -1001._eb
    top                   = 0._eb
    type                  = 'NULL'
    width                 = 0._eb
    height                = 0._eb

    end subroutine set_defaults
    
    subroutine set_criterion
    
    if  (trim(criterion)=='TIME' .or. trim(criterion)=='TEMPERATURE' .or. trim(criterion)=='FLUX') then
        if (trim(criterion)=='TIME') then
            ventptr%opening_type = trigger_by_time
            if (t(1)/=-1001._eb) then
                do ipts = 1,mxpts
                    if (t(ipts)/=-1001._eb) then
                        ventptr%t(ipts) = t(ipts)
                        ventptr%f(ipts) = f(ipts)
                        ventptr%npoints = ventptr%npoints + 1
                    end if
                end do
            end if
        else
            if (trim(criterion)=='TEMPERATURE') then
                ventptr%opening_type = trigger_by_temp 
                ventptr%opening_criterion = setpoint + kelvin_c_offset
            else if (criterion=='FLUX') then
                ventptr%opening_type = trigger_by_flux
                ventptr%opening_criterion = setpoint * 1000._eb
            end if
            ventptr%f(1) = pre_fraction
            ventptr%f(2) = post_fraction
            ventptr%npoints = 2
            ventptr%opening_target = 0
            do ic = 1,n_targets
                targptr => targetinfo(ic)
                if (trim(targptr%id)==trim(devc_id)) ventptr%opening_target = ic
            end do
            if (ventptr%opening_target==0) then
                write (errormessage,'(4a,i0)') '***Error, Vent opening specification requires an associated target.', &
                    ' for vent ',trim(id), ' &VENT number ', counter1 + counter2 + counter3
                call cfastexit('read_vent_set_criterion',1)
            end if
            if (stpmax>0) then
                stpmax = min(stpmax,1.0_eb)
            else
                stpmax = 1.0_eb
            end if
        end if
    else
        write (errormessage,'(4a,i0)') '***Error, Inputs for wall vent: criterion has to be "TIME", "TEMPERATURE", or "FLUX".', &
            ' for vent ',trim(id), ' &VENT number ', counter1 + counter2 + counter3
        call cfastexit('read_vent_set_criterion',2)
    end if
    
    end subroutine set_criterion
    
    subroutine find_comp_idxes

    i=0
    j=0

    do mm = 1, 2
        iroom=-101
        compartment_id=' '
        compartment_id=trim(comp_ids(mm))

        searching: do jj=1,n_rooms
            roomptr => roominfo(jj)
            if (trim(compartment_id) == 'OUTSIDE') then
                iroom = n_rooms+1
                exit searching
            end if
            if (trim(compartment_id) == trim(roomptr%id)) then
                iroom = roomptr%compartment
                exit searching
            end if
        end do searching

        if (iroom == -101) then
            write (errormessage,'(5a,i0)') '***Error, COMP_IDS do not specify existing compartments. ', comp_ids(mm), &
                ' for vent ',trim(id), ' &VENT number ', counter1 + counter2 + counter3
        end if

        if (mm == 1) i = iroom
        if (mm == 2) j = iroom
    end do
    
    end  subroutine find_comp_idxes

    end subroutine read_vent


    ! --------------------------- read_conn -------------------------------------------
    
!> \brief   read in &CONN namelist that includes surface heat transfer connection specifications
    
!> \param   lu (input): logical input unit number for the open input file
    
    subroutine read_conn (lu)

    integer, intent(in) :: lu

    integer :: ios, ifrom, ito, i, k, jj, i1, i2, counter1
    real(eb), dimension(mxpts) :: frac
    character(len=64) :: compartment_id
    integer :: nmlcount                             ! count of number of each namelist type read in so far

    type(room_type), pointer :: roomptrfrm, roomptrto

    real(eb), dimension(mxpts) :: f
    character(len=64), dimension(mxpts) :: comp_ids
    character(len=64) :: comp_id, type
    namelist /CONN/ comp_id, comp_ids, f, type

    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0

    ! Scan entire file to look for 'CONN'
    nmlcount = 0
    conn_loop: do
        call checkread ('CONN', lu, ios)
        if (ios==0) connflag=.true.
        if (ios==1) then
            exit conn_loop
        end if
        read(lu,CONN,err=34,iostat=ios)
        if (trim(type) == trim('CEILING') .or. trim(type) == trim('FLOOR')) n_vcons = n_vcons + 1
        if (trim(type) == trim('WALL')) nmlcount = nmlcount + 1
34      if (ios>0) then
            write(errormessage, '(a,i0)') 'Error, Invalid specification in &CONN inputs. Check &CONN input, ' , n_vcons+nmlcount
            call cfastexit('read_conn',1)
        end if
    end do conn_loop

    conn_flag: if (connflag) then

        rewind (lu)
        input_file_line_number = 0

        counter1 = 0

        countloop : do k = 1, nmlcount + n_vcons

            call checkread('CONN',lu,ios)
            call set_defaults
            read(lu,CONN)

            if (trim(type) == 'WALL') then
                frac(:)=-101
                compartment_id = ' '
                compartment_id = comp_id
                ifrom = -101

                searching: do jj=1,n_rooms
                    roomptrfrm => roominfo(jj)
                    if (trim(compartment_id) == trim(roomptrfrm%id)) then
                        ifrom = roomptrfrm%compartment
                        exit searching
                    end if
                end do searching

                if (ifrom == -101) then
                    write (errormessage,'(2a)') '***Error, Compartment not found for from room. ', comp_id
                    call cfastexit('read_conn',2)
                end if

                roomptrfrm => roominfo(ifrom)
                roomptrfrm%iheat = 2

                frac(:) = f(:)

                do i = 1, count(frac /= -1001._eb)
                    compartment_id = ' '
                    compartment_id = comp_ids(i)
                    ito=-101

                   searching_2: do jj=1,n_rooms
                        roomptrto => roominfo(jj)
                        if (trim(compartment_id) == 'OUTSIDE') then
                            ito = n_rooms+1
                            exit searching_2
                        end if
                        if (trim(compartment_id) == trim(roomptrto%id)) then
                            ito = roomptrto%compartment
                            exit searching_2
                        end if
                    end do searching_2

                    if (ito == -101) then
                        write (errormessage,'(2a)') '***Error, COMP_IDS do not match existing compartments. ', comp_ids(i)
                        call cfastexit('read_conn',3)
                    end if

                    if (ito<1.or.ito==ifrom.or.ito>n_rooms+1) then
                        write (errormessage, 5356) ifrom, ito
                        call cfastexit('read_conn',4)
                    end if
                    if (f(i)<0.0_eb.or.f(i)>1.0_eb) then
                        write (errormessage, 5357) ifrom, ito, f(i)
                        call cfastexit('read_conn',5)
                    end if
                    roomptrfrm%heat_frac(ito) = f(i)
                end do

            else if (trim(type) == 'CEILING' .or. trim(type) == 'FLOOR') then
                counter1 = counter1 + 1

                compartment_id = ' '
                compartment_id = comp_id
                i1 = -101

                searching_3: do jj = 1, n_rooms
                    roomptrfrm => roominfo(jj)
                    if (trim(compartment_id) == trim(roomptrfrm%id)) then
                        i1 = roomptrfrm%compartment
                        exit searching_3
                    end if
                end do searching_3

                if (i1 == -101) then
                    write (errormessage,'(a,a)') '***Error, COMP_ID not found. ', comp_id
                    call cfastexit('read_conn',6)
                end if

                compartment_id = ' '
                compartment_id = comp_ids(1)
                i2 = -101

                searching_4: do jj = 1, n_rooms
                    roomptrfrm => roominfo(jj)
                    if (trim(compartment_id) == trim(roomptrfrm%id)) then
                        i2 = roomptrfrm%compartment
                        exit searching_4
                    end if
                end do searching_4

                if (i2 == -101) then
                    write (errormessage,'(2a)') '***Error, Compartment not found for to room. ', comp_ids(1)
                    call cfastexit('read_conn',7)
                end if

                if (i1<1.or.i2<1.or.i1>n_rooms+1.or.i2>n_rooms+1) then
                    write (errormessage,5345) i1, i2
                    call cfastexit('read_conn',8)
                end if

                vertical_connections(counter1,w_from_room) = i1
                vertical_connections(counter1,w_from_wall) = 2
                vertical_connections(counter1,w_to_room) = i2
                vertical_connections(counter1,w_to_wall) = 1
            end if

        end do countloop

    end if conn_flag

5356 format ('***Error, Bad CONN input. CONN specification error in compartment pairs: ',2i3)
5357 format ('***Error, Bad CONN input. Error in fraction for CONN:',2i3,f6.3)
5345 format ('***Error, Bad VHEAT input. A referenced compartment does not exist')



    contains

    subroutine set_defaults

    comp_id           = 'NULL'
    comp_ids(:)       = 'NULL'
    f(:)              = -1001._eb
    type              = 'NULL'

    end subroutine set_defaults

    end subroutine read_conn


    ! --------------------------- read_isof --------------------------------------------
    
!> \brief   read in &ISOF namelist that includes isofurface specifications
    
!> \param   lu (input): logical input unit number for the open input file

    subroutine read_isof (lu)

    integer, intent(in) :: lu

    integer :: ios, ii, icomp, jj, counter
    character(len=64) :: compartment_id

    type(visual_type), pointer :: sliceptr
    type(room_type), pointer :: roomptr

    real(eb) :: value
    character(len=64) :: comp_id
    namelist /ISOF/ comp_id, value

    ios = 1
    counter = 0

    rewind (unit=lu)
    input_file_line_number = 0

    ! Scan entire file to look for 'ISOF'
    isof_loop: do
        call checkread ('ISOF',lu,ios)
        if (ios==0) isofflag=.true.
        if (ios==1) then
            exit isof_loop
        end if
        read(lu,ISOF,err=34,iostat=ios)
        counter = counter + 1
34      if (ios>0) then
            write(errormessage, '(a,i3)') 'Error, Invalid specification in &ISOF inputs. Check &ISOF input, ' , counter
            call cfastexit('read_isof',1)
        end if
    end do isof_loop

    isof_flag: if (isofflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_isof_loop: do ii = 1, counter

            call checkread('ISOF',lu,ios)
            call set_defaults
            read(lu,ISOF)

            compartment_id = ' '
            compartment_id = comp_id
            icomp = 0

            searching: do jj = 1, n_rooms
                roomptr => roominfo(jj)
                if (trim(compartment_id) == trim(roomptr%id)) then
                    icomp = roomptr%compartment
                    exit searching
                end if
            end do searching

            n_visual = n_visual + 1
            sliceptr => visualinfo(n_visual)
            sliceptr%vtype = 3
            sliceptr%value = value + kelvin_c_offset
            sliceptr%roomnum = icomp

            if (sliceptr%roomnum<0.or.sliceptr%roomnum>n_rooms) then
                write (errormessage, 5404) counter
                call cfastexit('read_isof',2)
            end if

        end do read_isof_loop

    end if isof_flag

5404 format ('***Error, Invalid ISOF specification in visualization input ',i0)



    contains

    subroutine set_defaults

    value                   = -1001.0_eb
    comp_id                 = 'NULL'

    end subroutine set_defaults

    end subroutine read_isof

    ! --------------------------- read_slcf --------------------------------------------
    
!> \brief   read in &SLCF namelist that includes slice file specifications
    
!> \param   lu (input): logical input unit number for the open input file
    
    subroutine read_slcf (lu)

    integer, intent(in) :: lu
    
    integer :: ios, ii, jj, icomp, counter
    character(len=64) :: compartment_id

    type(room_type), pointer :: roomptr
    type(visual_type), pointer :: sliceptr

    real(eb) :: position
    character(len=64) :: domain,plane
    character(len=64) :: comp_id
    namelist /SLCF/ domain, plane, position, comp_id

    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0
    counter = 0

    ! Scan entire file to look for 'SLCF'
    slcf_loop: do
        call checkread ('SLCF',lu,ios)
        if (ios==0) slcfflag=.true.
        if (ios==1) then
            exit slcf_loop
        end if
        read(lu,SLCF,err=34,iostat=ios)
        counter = counter + 1
34      if (ios>0) then
            write(errormessage, '(a,i0)') 'Error, Invalid specification in &SLCF inputs. Check &SLCF input, ' , counter
            call cfastexit('read_slcf',1)
        end if
    end do slcf_loop

    slcf_flag: if (slcfflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_slcf_loop: do ii = 1,counter

            call checkread('SLCF',lu,ios)
            call set_defaults
            read(lu,SLCF)

            n_visual = n_visual + 1
            sliceptr => visualinfo(n_visual)
            if (trim(domain)=='2-D') then
                sliceptr%vtype = 1
            else if (trim(domain)=='3-D') then
                sliceptr%vtype = 2
            else
                write (errormessage, 5403) counter
                call cfastexit('read_slcf',2)
            end if

            compartment_id = ' '
            compartment_id = comp_id
            icomp = 0

            if (trim(compartment_id) /= 'NULL') then
                searching: do jj = 1, n_rooms
                    roomptr => roominfo(jj)
                    if (trim(compartment_id) == trim(roomptr%id)) then
                        icomp = roomptr%compartment
                        exit searching
                    end if
                end do searching
            end if

            ! 2-D slice file
            if (sliceptr%vtype==1) then
                ! get position (required) and compartment (optional) first so we can check to make sure
                ! desired position is within the compartment(s)
                sliceptr%position = position
                sliceptr%roomnum  = icomp
                if (sliceptr%roomnum<0.or.sliceptr%roomnum>n_rooms) then
                    write (errormessage,5403) counter
                    call cfastexit('read_slcf',3)
                end if
                if (trim(plane) =='X') then
                    sliceptr%axis = 1
                    if (sliceptr%roomnum>0) then
                        roomptr => roominfo(sliceptr%roomnum)
                        if (sliceptr%position>roomptr%cwidth.or.sliceptr%position<0.0_eb) then
                            write (errormessage, 5403) counter
                            call cfastexit('read_slcf',4)
                        end if
                    end if
                else if (trim(plane) =='Y') then
                    sliceptr%axis = 2
                    if (sliceptr%roomnum>0) then
                        roomptr => roominfo(sliceptr%roomnum)
                        if (sliceptr%position>roomptr%cdepth.or.sliceptr%position<0.0_eb) then
                            write (errormessage, 5403) counter
                            call cfastexit('read_slcf',5)
                        end if
                    end if
                else if (trim(plane) =='Z') then
                    sliceptr%axis = 3
                    if (sliceptr%roomnum>0) then
                        roomptr => roominfo(sliceptr%roomnum)
                        if (sliceptr%position>roomptr%cheight.or.sliceptr%position<0.0_eb) then
                            write (errormessage, 5403) counter
                            call cfastexit('read_slcf',6)
                        end if
                    end if
                else
                    write (errormessage, 5403) counter
                    call cfastexit('read_slcf',7)
                end if

                ! 3-D slice
            else if (sliceptr%vtype==2) then
                sliceptr%roomnum = icomp
                if (sliceptr%roomnum<0.or.sliceptr%roomnum>n_rooms) then
                    write (errormessage, 5403) counter
                    call cfastexit('read_slcf',8)
                end if
            end if

        end do read_slcf_loop

    end if slcf_flag

5403 format ('***Error, Bad SLCF input. Invalid SLCF specification in visualization input ',i0)

    contains

    subroutine set_defaults

    domain                  = 'NULL'
    plane                   = 'NULL'
    position                = 0._eb
    comp_id                 = 'NULL'

    end subroutine set_defaults

    end subroutine read_slcf


    ! --------------------------- read_diag -------------------------------------------
    
!> \brief   read in &DIAG namelist that includes inputs to control diagnostic calculations and output
    
!> \param   lu (input): logical input unit number for the open input file
    
    subroutine read_diag (lu)

    integer :: ios, i
    integer, intent(in) :: lu

    character(len=8) :: mode
    character(len=3) :: horizontal_flow_sub_model, fire_sub_model, entrainment_sub_model, vertical_flow_sub_model, &
        ceiling_jet_sub_model, door_jet_fire_sub_model, convection_sub_model, radiation_sub_model, &
        conduction_sub_model, debug_print, mechanical_flow_sub_model, keyboard_input, &
        steady_state_initial_conditions, dassl_debug_print, oxygen_tracking, residual_debug_print, &
        layer_mixing_sub_model, adiabatic_target_verification
    character(len=10) :: gas_absorbtion_sub_model
    real(eb), dimension(mxpts) :: t, f
    real(eb) :: radiative_incident_flux
    namelist /DIAG/ mode, rad_solver, partial_pressure_h2o, partial_pressure_co2, gas_temperature, t, f,  &
        horizontal_flow_sub_model, fire_sub_model, entrainment_sub_model, vertical_flow_sub_model, &
        ceiling_jet_sub_model, door_jet_fire_sub_model, convection_sub_model, radiation_sub_model, &
        conduction_sub_model, debug_print, mechanical_flow_sub_model, keyboard_input, &
        steady_state_initial_conditions, dassl_debug_print, oxygen_tracking, gas_absorbtion_sub_model, &
        residual_debug_print, layer_mixing_sub_model, adiabatic_target_verification, radiative_incident_flux, &
        upper_layer_thickness, verification_time_step, verification_fire_heat_flux

    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0

    ! scan entire file to look for &DIAG input
    diag_loop: do
        call checkread ('DIAG',lu,ios)
        if (ios==0) diagflag=.true.
        if (ios==1) then
            exit diag_loop
        end if
        read(lu,DIAG,iostat=ios)
        if (ios>0) then
            write(errormessage, '(a)') '***Error in &DIAG: Invalid specification for inputs.'
            call cfastexit('read_diag',1)
        end if
    end do diag_loop

    ! we found one. read it (only the first one counts; others are ignored)
    diag_flag: if (diagflag) then

        rewind (lu)
        input_file_line_number = 0

        call checkread('DIAG',lu,ios)
        call set_defaults
        read(lu,DIAG)

        if (rad_solver == 'RADNNET') radi_radnnet_flag = .true.   
        
        if (upper_layer_thickness/=-1001._eb) radi_verification_flag = .true.
        if (partial_pressure_h2o/=-1001._eb) radi_verification_flag = .true.
        if (partial_pressure_co2/=-1001._eb) radi_verification_flag = .true.
        if (gas_temperature/=-1001._eb) then
            gas_temperature = gas_temperature + kelvin_c_offset
            radi_verification_flag = .true.
        end if
        if (verification_fire_heat_flux/=-1001._eb) &
            verification_fire_heat_flux = verification_fire_heat_flux * 1000._eb
        if (furn_temp(1)/=-1001._eb) then
            n_furn = 0
            do i = 1, mxpts
                if (t(i)/=-1001._eb) then
                    n_furn = n_furn + 1
                    furn_time(n_furn) = t(i)
                    furn_temp(n_furn) = f(i) + kelvin_c_offset
                end if
            end do
        end if
        if (fire_sub_model == 'OFF') then
            option(ffire) = off
        end if 
        if (horizontal_flow_sub_model == 'OFF') then
            option(fhflow) = off
        end if 
        if (entrainment_sub_model == 'OFF') then
            option(fentrain) = off
        end if 
        if (vertical_flow_sub_model == 'OFF') then
            option(fvflow) = off
        end if 
        if (ceiling_jet_sub_model == 'OFF') then
            option(fcjet) = off
        end if 
        if (door_jet_fire_sub_model == 'OFF') then
            option(fdfire) = off
        end if 
        if (convection_sub_model == 'OFF') then
            option(fconvec) = off
        end if 
        if (radiation_sub_model == 'OFF') then
            option(frad) = off
        end if 
        if (conduction_sub_model == 'OFF') then
            option(fconduc) = off
        end if 
        if (trim(debug_print) == 'ON') then
            option(fdebug) = on
        end if 
        if (mechanical_flow_sub_model == 'OFF') then
            option(fmflow) = off
        end if 
        if (keyboard_input == 'OFF') then
            option(fkeyeval) = off
        end if 
        if (trim(steady_state_initial_conditions) == 'ON') then
            option(fpsteady) = on
        end if 
        if (trim(dassl_debug_print) == 'ON') then
            option(fpdassl) = on
        end if 
        if (trim(oxygen_tracking) == 'ON') then
            option(ffire) = on
        end if 
        if (trim(gas_absorbtion_sub_model) == 'CONSTANT') then
            option(fgasabsorb) = off
        end if 
        if (trim(residual_debug_print) == 'ON') then
            option(fresidprn) = on
        end if 
        if (trim(layer_mixing_sub_model) == 'OFF') then
            option(flayermixing) = off
        end if         
        if (trim(adiabatic_target_verification) == 'ON') then 
            verification_ast = .true.
            radiative_incident_flux_AST = radiative_incident_flux*1000._eb ! W/m^2 is used in the calculation
        end if
    
    end if diag_flag
    
    if (radi_verification_flag) validation_flag = .true.

    contains

    subroutine set_defaults

    rad_solver                      = 'NULL'
    partial_pressure_h2o            = -1001._eb
    partial_pressure_co2            = -1001._eb
    gas_temperature                 = -1001._eb
    t                               = -1001._eb
    f                               = -1001._eb
    fire_sub_model                  = 'ON'
    horizontal_flow_sub_model       = 'ON'
    entrainment_sub_model           = 'ON'
    vertical_flow_sub_model         = 'ON'
    ceiling_jet_sub_model           = 'ON'
    door_jet_fire_sub_model         = 'ON'
    convection_sub_model            = 'ON'
    radiation_sub_model             = 'ON'
    conduction_sub_model            = 'ON'
    debug_print                     = 'OFF'
    mechanical_flow_sub_model       = 'ON'
    keyboard_input                  = 'ON'
    steady_state_initial_conditions = 'OFF'
    dassl_debug_print               = 'OFF'
    oxygen_tracking                 = 'OFF'
    gas_absorbtion_sub_model        = 'CALCULATED'
    residual_debug_print            = 'OFF'
    layer_mixing_sub_model          = 'ON'
    adiabatic_target_verification   = 'OFF'
    radiative_incident_flux         = 0._eb
    upper_layer_thickness           = -1001._eb
    verification_time_step          = 0._eb

    end subroutine set_defaults

    end subroutine read_diag

    ! --------------------------- read_dump --------------------------------------------
    
!> \brief   read in &DUMP namelist that includes optional output specifications
    
!> \param   lu (input): logical input unit number for the open input file
    
    subroutine read_dump (lu)

    integer, intent(in) :: lu
    
    integer :: ios, i, ii, counter
    type(dump_type), pointer :: dumpptr
    logical :: found
    
    real(eb) :: criterion
    character(len=4) :: dump_syntax
    character(len=25) :: file, type
    character(len=64) :: id, first_device, first_measurement, second_device, second_measurement
    character(len=64), dimension(2) :: first_field, second_field
    character(len=128) :: fyi

    namelist /DUMP/ id, file, first_device, first_measurement, second_device, &
                    second_measurement, first_field, second_field, criterion, type, fyi
    namelist /OUTP/ id, file, first_device, first_measurement, second_device, &
                    second_measurement, first_field, second_field, criterion, type, fyi


    ios = 1
    rewind (unit=lu)
    input_file_line_number = 0
    counter = 0
    dump_syntax = "NULL"

    ! Scan entire file to look for 'DUMP'
    dump_loop: do
        call checkread ('DUMP',lu,ios)
        if (ios==0) then
            dumpflag=.true.
            dump_syntax = 'DUMP'
            read(lu,DUMP,err=34,iostat=ios)
            counter = counter + 1
34          if (ios>0) then
                write(errormessage, '(a,i3)') 'Error, Invalid specification in &DUMP inputs. Check &DUMP number ' , counter+1
                call cfastexit('read_dump',1)
            end if
        else if (ios==1) then
            exit dump_loop
        end if
    end do dump_loop
    if (.not.dumpflag.and.counter==0) then
        ios = 1
        rewind (unit=lu)
        input_file_line_number = 0
        counter = 0
        ! Scan entire file to look for 'OUTP'
        outp_loop: do
            call checkread ('OUTP',lu,ios)
            if (ios==0) then
                dumpflag=.true.
                dump_syntax = 'OUTP'
                read(lu,OUTP,err=35,iostat=ios)
                counter = counter + 1
35              if (ios>0) then
                    write(errormessage, '(a,i3)') 'Error, Invalid specification in &OUTP inputs. Check &OUTP number ' , counter+1
                    call cfastexit('read_dump',1)
                end if
            else if (ios==1) then
                exit outp_loop
            end if
        end do outp_loop
    end if

    if (dumpflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_dump_loop: do ii = 1,counter

            call checkread(dump_syntax,lu,ios)
            call set_defaults
            if (dump_syntax=='DUMP') read(lu,DUMP)
            if (dump_syntax=='OUTP') read(lu,OUTP)
            
            if (first_field(1)==' ' .and. first_field(2)==' ' .and. first_device/=' ' .and. first_measurement/=' ') then
                first_field(1) = first_device
                first_field(2) = first_measurement
            end if
            if (second_field(1)==' ' .and. second_field(2)==' ' .and. second_device/=' ' .and. second_measurement/=' ') then
                second_field(1) = second_device
                second_field(2) = second_measurement
            end if
    
            if (id == ' ') then
                write(errormessage,'(3a,i0)') 'Error in &',dump_syntax,': ID must be defined number ', counter
                call cfastexit('read_dump',2)
            end if

            if (.not.newid(id)) then
                write(errormessage,'(5a,i3)') '***Error, Not a unique identifier for &',dump_syntax,': ',trim(id), &
                    'Check dump ',counter
                call cfastexit('read_dump',3)
            end if
            found = .false.
            do i = 1, num_csvfiles
                if (trim(file)==trim(csvnames(i))) then
                    found = .true.
                end if
            end do
            if (.not.found) then
                write(errormessage,'(5a,i0)') 'Error in &',dump_syntax,': Invalid specification for FILE ',trim(file), &
                    ' number ',counter
                call cfastexit('read_dump',4)
            end if
            if (.not.((type(1:3)=='MIN').or.(type(1:3)=='MAX').or. &
                    (type(1:8)=='TRIGGER_').or.(type(1:9)=='INTEGRATE').or. &
                    (type(1:15)=='CHECK_TOTAL_HRR'))) then
                write(errormessage,'(5a,i0)') 'Error in &',dump_syntax,': Invalid specification for type ',trim(type), &
                    ' number ',counter
                call cfastexit('read_dump',5)
            end if
            if (first_field(1)==' ') then
                write(errormessage,'(3a,i0)') 'Error in &',dump_syntax,': FIRST_FIELD must be defined, number ',counter
                call cfastexit('read_dump',6)
            end if 
            if (first_field(2)==' ') then
                write(errormessage,'(3a,i0)') 'Error in &',dump_syntax,': FIRST_FIELD must be defined, number ',counter
                call cfastexit('read_dump',7)
            end if 
            if ((type(1:8)=='TRIGGER_').and.(second_field(1)==' ')) then
                write(errormessage,'(5a,i0)') 'Error in &',dump_syntax,': SECOND_FIELD must be defined for type ', trim(type), &
                    ', number ',counter
                call cfastexit('read_dump',8)
            end if
            if ((type(1:8)=='TRIGGER_').and.(second_field(2)==' ')) then
                write(errormessage,'(5a,i0)') 'Error in &',dump_syntax,': SECOND_FIELD must be defined for type ', trim(type), &
                    ', number ',counter
                call cfastexit('read_dump',9)
            end if
            if ((type(1:9)=='INTEGRATE').and.(second_field(2)==' ')) then
                write(errormessage,'(5a,i0)') 'Error in &',dump_syntax,': SECOND_FIELD must be defined for type ', trim(type), &
                    ', number ',counter
                call cfastexit('read_dump',10)
            end if
            if ((trim(type)=='INTEGRATE').and.(second_field(2)==' ')) then
                write(errormessage,'(5a,i0)') 'Error in &',dump_syntax,': SECOND_FIELD must be defined for type ', trim(type), &
                    ', number ',counter
                call cfastexit('read_dump',11)
            end if
            if ((type(1:9)=='INTEGRATE').and.(first_field(1)(1:4)/='Time')) then
                write(errormessage,'(5a,i0)') 'Error in &',dump_syntax,': FIRST_FIELD must be defined as Simulation Time for ', &
                    trim(type),', number ',counter
                call cfastexit('read_dump',12)
            end if
            if ((type(1:9)=='INTEGRATE').and.(first_field(1)(1:15)/='Simulation Time')) then
                write(errormessage,'(5a,i0)') 'Error in &',dump_syntax,': FIRST_FIELD must be defined as Time for ', &
                    trim(type),' number ',counter
                call cfastexit('read_dump',13)
            end if
            if ((type(1:8)=='TRIGGER_').and.(criterion<=0)) then
                write(errormessage,'(3a,i0)') 'Error in  &',dump_syntax,': for a TRIGGER analysis CRITERION must be > 0 number ', &
                    counter
                call cfastexit('read_dump',14)
            end if
            
            n_dumps = n_dumps + 1
            if (n_dumps>mx_dumps) then
                write(errormessage,'(5a,i0)') 'Error in &',dump_syntax,': Too many &',dump_syntax,' entries. Maximum is', mx_dumps
                call cfastexit('read_dump',15)
            end if
            dumpptr => dumpinfo(n_dumps)
            dumpptr%id = id
            dumpptr%fyi = fyi
            if (type(1:15) == 'CHECK_TOTAL_HRR') then
                dumpptr%file = 'NORMAL'
                dumpptr%type = type
                dumpptr%first_field(1) = 'Time'
                dumpptr%first_field(2) = 'Simulation Time'
                dumpptr%second_field = first_field
                dumpptr%criterion = 0 
                dumpptr%relative_column = n_dumps
                dumpptr%found = .false.
            else
                dumpptr%file = file
                dumpptr%type = type
                dumpptr%first_field(1) = first_field(1)
                dumpptr%first_field(2) = first_field(2)
                dumpptr%second_field(1) = second_field(1)
                dumpptr%second_field(2) = second_field(2)
                dumpptr%criterion = criterion
                dumpptr%relative_column = n_dumps
                dumpptr%found = .false.
            end if 

        end do read_dump_loop

    end if
    return

    contains

    subroutine set_defaults

    id = ' '
    fyi = ' '
    file = ' '
    first_field = ' '
    second_field = (/' ', ' '/)
    first_device = ' '
    first_measurement = ' '
    second_device = ' '
    second_measurement = ' '
    type = ' '
    criterion = -1
    
    end subroutine set_defaults

    end subroutine read_dump
    
    ! --------------------------- checkread ---------------------------------------
    
!> \brief   read input file checking for the next specified namelist input.
!>          on successful search the file is left at the line before the found item so the input routine can read it

!> \param   name (input): namelist to  be looked for
!> \param   lu (input): logicial unit number of the open input file to be read
!> \param   ios (output): status returned. 0 if OK, 1 if error
    
    subroutine checkread(name,lu,ios)

    ! look for the namelist variable name and then stop at that line.

    integer :: ii
    integer, intent(out) :: ios
    integer, intent(in) :: lu
    character(len=4), intent(in) :: name
    
    character(len=80) text
    ios = 1
    input_file_line = ''

    readloop: do
        read(lu,'(a)',end=10) text
        input_file_line_number = input_file_line_number + 1
        input_file_line = text
        tloop: do ii=1,72
            if (text(ii:ii)/='&' .and. text(ii:ii)/=' ') exit tloop
            if (text(ii:ii)=='&') then
                if (text(ii+1:ii+4)==name) then
                    backspace(lu)
                    ios = 0
                    exit readloop
                else
                    cycle readloop
                endif
            endif
        enddo tloop
    enddo readloop

10  return

    end subroutine checkread
    
    !-------------------------------newid--------------------------------------

!> \brief   check for an existing id to make sure all ids in the file are unique. Returns false if id already exists
    
!> \param   id (input) name to be checked against all existing ids
    
    logical function newid(id)
    
    character(len=*), intent(in) :: id
    
    integer :: i
    
    do i = 1, n_rooms
        if (trim(id)==trim(roominfo(i)%id)) then
            newid = .false.
            return
        end if
    enddo
    do i = 1, n_matl
        if (trim(id)==trim(material_info(i)%id)) then
            newid = .false.
            return
        end if
    enddo
    do i = 1, n_hvents
        if (trim(id)==trim(hventinfo(i)%id)) then
            newid = .false.
            return
        end if
    enddo
    do i = 1, n_leaks
        if (trim(id)==trim(leakinfo(i)%id)) then
            newid = .false.
            return
        end if
    enddo
    do i = 1, n_vvents
        if (trim(id)==trim(vventinfo(i)%id)) then
            newid = .false.
            return
    end if
    enddo
    do i = 1, n_mvents
        if (trim(id)==trim(mventinfo(i)%id)) then
            newid = .false.
            return
        end if
    enddo
    do i = 1, n_detectors
        if (trim(id)==trim(detectorinfo(i)%id)) then
            newid = .false.
            return
        end if
    enddo
    do i = 1, n_targets
        if (trim(id)==trim(targetinfo(i)%id)) then
            newid = .false.
            return
        end if
    enddo
    do i = 1, n_fires
        if (trim(id)==trim(fireinfo(i)%id)) then
            newid = .false.
            return
        end if
    enddo
    
    newid = .true.
    return
    
    end function newid
    
    !---------------------cdata_rereadinputfile---------------------------
    
!> \brief reread CFAST input file for CData file generation
    
    subroutine cdata_preprocessor_rereadinputfile
    
    integer :: ios
    character(len=256) :: buf
    
    !open input file and check to see if it's a new (namelist) format file
    open (newunit=iofili, file=inputfile, action='read', status='old', iostat=ios)
    read (unit=iofili,fmt='(a)') buf
    rewind (unit=iofili)
    
    init_fire = .true.
    init_vent = .true.
    init_devc = .true.
    call initialize_memory
    
    convert_negative_distances = .false.
    call read_devc(iofili)
    call read_tabl(iofili)
    call read_fire(iofili)
    call read_vent(iofili)
    convert_negative_distances = .true. 
    
    close(iofili)
    
    end subroutine cdata_preprocessor_rereadinputfile

    end module namelist_input_routines
