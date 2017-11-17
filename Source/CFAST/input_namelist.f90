    module namelist_input_routines

    use precision_parameters

    use fire_routines, only: flame_height
    use utility_routines, only: upperall, set_heat_of_combustion, position_object

    use wallptrs
    use cenviro
    use ramp_data
    use cparams
    use defaults
    use setup_data
    use detectorptrs
    use target_data
    use fire_data
    use solver_data
    use smkview_data
    use thermal_data
    use vent_data
    use room_data
    use namelist_data

    implicit none 
    
    private

    public namelist_input

    contains
    ! --------------------------- namelist_input ----------------------------------
    subroutine namelist_input

    implicit none

    integer :: ncomp

    ncomp = 0
    nvisualinfo=0

    call read_head(iofili)
    call read_time(iofili)
    call read_init(iofili)
    call read_misc(iofili)
    call read_matl(iofili)
    call read_ramp(iofili)
    call read_comp(iofili,ncomp)
    call read_devc(iofili)
    call read_fire(iofili)
    call read_vent(iofili)
    call read_conn(iofili)
    call read_isof(iofili)
    call read_slcf(iofili)

    close (iofili)
    
    return

    ! read format list
5050 format ('***Error: Error opening the input file = ',I6)


    end subroutine namelist_input


    ! --------------------------- head --------------------------------------
    subroutine read_head(lu)

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
            write(iofill, '(a)') '***Error in &HEAD: Invalid specification for inputs.'
            stop
        end if
    end do head_loop

    if (.not.headflag) then
        write (*, '(/, "***Error: &HEAD inputs are required.")')
        write (iofill, '(/, "***Error: &HEAD inputs are required.")')
        stop
    end if

    ! we found one. read it (only the first one counts; others are ignored)
    head_flag: if (headflag) then

        rewind (lu)
        input_file_line_number = 0

        call checkread('HEAD',lu,ios)
        call set_head_defaults
        read(lu,HEAD)

        version = version/1000

    end if head_flag

    if (version/=cfast_version/1000) then
        write (*,5002) version, cfast_version/1000
        write (iofill,5002) version, cfast_version/1000
    end if

5002 format ('***Warning: Input file written for CFAST version', i3, ' running on CFAST version', i3)

    contains

    subroutine set_head_defaults

    version = default_version

    end subroutine set_head_defaults

    end subroutine read_head


    ! --------------------------- time -------------------------------------------
    
    subroutine read_time(lu)

    integer :: ios
    integer, intent(in) :: lu

    integer :: simulation,print,spreadsheet,smokeview
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
            write(iofill, '(a)') '***Error in &TIME: Invalid specification for inputs.'
            stop
        end if
    end do time_loop

    if (.not.timeflag) then
        write (*, '(/, "***Error: &TIME inputs are required.")')
        write (iofill, '(/, "***Error: &TIME inputs are required.")')
        stop
    end if

    ! we found one. read it (only the first one counts; others are ignored)
    time_flag: if (timeflag) then

        rewind (lu)
        input_file_line_number = 0

        call checkread('TIME',lu,ios)
        call set_time_defaults
        read(lu,TIME)

        time_end=simulation
        print_out_interval=print
        smv_out_interval=smokeview
        ss_out_interval=spreadsheet

    end if time_flag

    contains

    subroutine set_time_defaults

    simulation              = default_simulation_time    ! s
    print                   = default_print_out_interval ! s
    smokeview               = default_smv_out_interval   ! s
    spreadsheet             = default_ss_out_interval    ! s

    end subroutine set_time_defaults

    end subroutine read_time

    ! --------------------------- init ------------------------------------------
    
    subroutine read_init(lu)

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
            write(iofill, '(a)') '***Error in &INIT: Invalid specification for inputs.'
            stop
        end if
    end do init_loop

    init_flag: if (initflag) then

        rewind (lu)
        input_file_line_number = 0

        call checkread('INIT',lu,ios)
        call set_init_defaults
        read(lu,INIT)

        exterior_ambient_temperature  = exterior_temperature + kelvin_c_offset
        interior_ambient_temperature  = interior_temperature + kelvin_c_offset
        exterior_abs_pressure = pressure
        interior_abs_pressure = exterior_abs_pressure*exterior_ambient_temperature/interior_ambient_temperature
        relative_humidity     = relative_humidity*0.01_eb

        tgignt = interior_ambient_temperature + 200.0_eb

    end if init_flag

    contains

    subroutine set_init_defaults

    exterior_temperature     = default_temperature - kelvin_c_offset    ! C
    interior_temperature     = default_temperature - kelvin_c_offset    ! C
    pressure                 = default_pressure                         ! Pa
    relative_humidity        = default_relative_humidity*100._eb        ! %

    end subroutine set_init_defaults

    end subroutine read_init


    ! --------------------------- misc -------------------------------------------
    subroutine read_misc(lu)

    integer, intent(in) :: lu

    integer :: ios

    real(eb) :: max_time_step, lower_oxygen_limit
    character(64) :: adiabatic
    namelist /MISC/ adiabatic, max_time_step, lower_oxygen_limit

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
            write(iofill, '(a)') '***Error in &MISC: Invalid specification for inputs.'
            stop
        end if
    end do misc_loop

    misc_flag: if (miscflag) then

        rewind (lu)
        input_file_line_number = 0

        call checkread('MISC',lu,ios)
        call set_misc_defaults
        read(lu,MISC)

        adiabatic_walls=.false.
        if (adiabatic == '.TRUE.') adiabatic_walls=.true.
        stpmax = max_time_step
        lower_o2_limit = lower_oxygen_limit

    end if misc_flag

    contains

    subroutine set_misc_defaults

    ! note actual default values are set in initialize_memory and used here to initialize namelist

    adiabatic = '.FALSE.'
    max_time_step = stpmax                          ! s
    lower_oxygen_limit = default_lower_oxygen_limit

    end subroutine set_misc_defaults

    end subroutine read_misc


    ! --------------------------- MATL -------------------------------------------
    subroutine read_matl(lu)


    integer :: ios,ii
    integer, intent(in) :: lu
    type(thermal_type), pointer :: thrmpptr

    real(eb) :: conductivity, density, emissivity, specific_heat, thickness
    character(64) :: id, material
    namelist /MATL/ conductivity, density, emissivity, id, material, specific_heat, thickness

    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0

    ! Scan entire file to look for 'MATL'
    n_thrmp = 0
    matl_loop: do
        call checkread ('MATL',lu,ios)
        if (ios==0) matlflag=.true.
        if (ios==1) then
            exit matl_loop
        end if
        read(lu,MATL,iostat=ios)
        n_thrmp = n_thrmp + 1
        if (ios>0) then
            write(iofill, '(a,i3)') '***Error in &MATL: Invalid specification for inputs. Check &MATL input, ' , n_thrmp
            stop
        end if
    end do matl_loop

    if (n_thrmp>mxthrmp) then
        write (*,'(a,i3)') '***Error: Too many thermal properties in input data file. Limit is ', mxthrmp
        write (iofill,'(a,i3)') '***Error: Too many thermal properties in input data file. Limit is ', mxthrmp
        stop
    end if

    matl_flag: if (matlflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_matl_loop: do ii=1,n_thrmp

            thrmpptr => thermalinfo(ii)

            call checkread('MATL',lu,ios)
            call set_matl_defaults
            read(lu,MATL)

            thrmpptr%name          = id
            thrmpptr%nslab         = 1
            thrmpptr%k(1)          = conductivity
            thrmpptr%c(1)          = specific_heat*1e3
            thrmpptr%rho(1)        = density
            thrmpptr%thickness(1)  = thickness
            thrmpptr%eps           = emissivity

        end do read_matl_loop

    end if matl_flag


    contains

    subroutine set_matl_defaults

    specific_heat          = 0.0_eb        !j/kg-k
    emissivity             = 0.9_eb
    conductivity           = 0.0_eb        !w/m-k
    id                     = 'NULL'
    density                = 0.0_eb        !kg/m3
    thickness              = 0.0_eb        !m

    end subroutine set_matl_defaults

    end subroutine read_matl


    ! --------------------------- COMP -------------------------------------------
    subroutine read_comp(lu,ncomp)

    integer, intent(in) :: lu
    integer, intent(inout) :: ncomp
    
    integer :: ios, ii, i, kk
    character :: tcname*64

    type(ramp_type), pointer :: rampptr
    type(room_type), pointer :: roomptr

    integer,dimension(3) :: grid
    real(eb) :: depth, height ,width
    real(eb),dimension(3) :: origin
    character(64) :: hall, shaft
    character(64) :: id, room_area_ramp, ceiling_matl_id, floor_matl_id, wall_matl_id
    namelist /COMP/ depth, grid, hall, height, id, ceiling_matl_id, floor_matl_id, wall_matl_id, origin, &
        room_area_ramp, shaft, width

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
            write(iofill, '(a,i3)') '***Error in &COMP: Invalid specification for inputs. Check &COMP input, ' , ncomp
            stop
        end if
        ncomp = ncomp + 1
    end do comp_loop

    if (ncomp>mxrooms) then
        write (*,'(a,i3)') '***Error: Too many compartments in input data file. Limit is ', mxrooms
        write (iofill,'(a,i3)') '***Error: Too many compartments in input data file. Limit is ', mxrooms
        stop
    end if

    if (.not.compflag) then
        write (*, '(/, "***Error: &COMP inputs are required.")')
        write (iofill, '(/, "***Error: &COMP inputs are required.")')
        stop
    end if

    comp_flag: if (compflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_comp_loop: do ii = 1, ncomp

            roomptr => roominfo(ii)

            call checkread('COMP',lu,ios)
            call set_comp_defaults
            read(lu,COMP)

            if (trim(room_area_ramp) /= 'NULL') then
                ramp_search: do kk = 1, nramps
                    rampptr=>rampinfo(kk)
                    if (trim(rampptr%id) == trim(room_area_ramp)) then
                        rampptr%room1 = ii
                        rampptr%room2 = ii
                        rampptr%counter = kk
                        do i = 1, rampptr%npoints
                            roomptr%var_height(i) = rampptr%x(i)
                            roomptr%var_area(i)   = rampptr%f_of_x(i)
                        end do
                        exit ramp_search
                    end if
                end do ramp_search

                if (kk == nramps+1) then
                    write (*,'(a,a,a)') '***Error in &COMP: COMP_AREA_RAMP_ID: ', room_area_ramp, ', not found.'
                    write (iofill,'(a,a,a)') '***Error in &COMP: COMP_AREA_RAMP_ID: ', room_area_ramp, ', not found.'
                    stop
                end if
            end if

            roomptr%compartment    = ii
            roomptr%name    = id
            roomptr%cwidth  = width
            roomptr%cdepth  = depth
            roomptr%cheight = height
            roomptr%x0 = origin(1)
            roomptr%y0 = origin(2)
            roomptr%z0 = origin(3)

            ! ceiling
            tcname = ceiling_matl_id
            if (trim(tcname)/='OFF') then
                roomptr%surface_on(1) = .true.
                roomptr%matl(1) = tcname
            end if

            ! floor
            tcname = floor_matl_id
            if (trim(tcname)/='OFF') then
                roomptr%surface_on(2) = .true.
                roomptr%matl(2) = tcname
            end if

            ! walls
            tcname = wall_matl_id
            if (trim(tcname)/='OFF') then
                roomptr%surface_on(3) = .true.
                roomptr%matl(3) = tcname
                roomptr%surface_on(4) = .true.
                roomptr%matl(4) = tcname
            end if

            roomptr%ibar = grid(1)
            roomptr%jbar = grid(2)
            roomptr%kbar = grid(3)

            roomptr%shaft=.false.
            roomptr%hall=.false.
            if (shaft == '.TRUE.') roomptr%shaft = .true.
            if (hall == '.TRUE.') roomptr%hall = .true.

        end do read_comp_loop

        nr = ncomp + 1

    end if comp_flag

    contains

    subroutine set_comp_defaults

    ceiling_matl_id         = 'OFF'
    id                      = 'NULL'
    depth                   = 0.0_eb
    floor_matl_id           = 'OFF'
    height                  = 0.0_eb
    wall_matl_id            = 'OFF'
    width                   = 0.0_eb
    grid(:)                 = default_grid
    origin(:)               = 0.0_eb
    room_area_ramp          = 'NULL'
    hall                    = '.FALSE.'
    shaft                   = '.FALSE.'

    end subroutine set_comp_defaults

    end subroutine read_comp


    ! --------------------------- DEVC -------------------------------------------
    subroutine read_devc (lu)

    integer, intent(in) :: lu
    
    integer :: ios
    integer :: iroom, ii, jj ,i1, counter1, counter2
    character(64) :: compartment_id
    character :: tcname*64
    logical :: idcheck

    type(room_type), pointer :: roomptr
    type(target_type), pointer :: targptr
    type(detector_type), pointer :: dtectptr

    real(eb) :: temperature_depth,rti,setpoint,spray_density
    real(eb),dimension(3) :: location,normal
    character(64) :: comp_id,id,matl_id
    character(64) :: type
    namelist /DEVC/ comp_id, type, id, temperature_depth, location, matl_id, normal, rti, setpoint, spray_density

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
        if (type == 'SPRINKLER' .or. type == 'HEAT_DETECTOR'.or. type == 'SMOKE_DETECTOR') n_detectors =n_detectors + 1
34      if (ios>0) then
            write(iofill, '(a,i3)') '***Error in &DEVC: Invalid specification for inputs. Check &DEVC input, ' , &
                n_targets+n_detectors
            stop
        end if
    end do devc_loop

    if (n_targets>mxtarg) then
        write (*,'(a,i3)') '***Error: Too many targets in input data file. Limit is ', mxtarg
        write (iofill,'(a,i3)') '***Error: Too many targets in input data file. Limit is ', mxtarg
        stop
    end if

    if (n_detectors>mxdtect) then
        write (*,'(a,i3)') '***Error: Too many detectors in input data file. Limit is ', mxdtect
        write (iofill,'(a,i3)') '***Error: Too many detectors in input data file. Limit is ', mxdtect
        stop
    end if

    devc_detec_flag: if (devcflag) then

        rewind (lu)
        input_file_line_number = 0

        counter1 = 0
        counter2 = 0
        ! Assign value to CFAST variables for further calculations
        read_devc_loop: do ii=1 , n_targets + n_detectors

            call checkread('DEVC',lu,ios)
            call set_devc_defaults
            read(lu,DEVC)

            if (trim(type) == 'PLATE' .or. trim(type) == 'CYLINDER') then
                counter1 = counter1 + 1

                targptr => targetinfo(counter1)

                iroom=0
                compartment_id = 'NULL'
                compartment_id = trim(comp_id)

                idcheck=.false.
                searching: do jj = 1, nr-1
                    roomptr => roominfo(jj)
                    if (trim(compartment_id) == trim(roomptr%name)) then
                        iroom = roomptr%compartment
                        idcheck = .true.
                        exit searching
                    end if
                end do searching

                if (.not. idcheck) then
                    write (*,'(a,a,a,i3)') '***Error in &DEVC: COMP_ID: ', id, ', not found. Check target, ', counter1
                    write (iofill,'(a,a,a,i3)') '***Error in &DEVC: COMP_ID: ', id, ', not found. Check target, ', counter1
                    stop
                end if

                if (iroom<1.or.iroom>nr) then
                    write (*,5003) iroom
                    write (iofill,5003) iroom
                    stop
                end if

                targptr%room = iroom

                ! position and normal vector
                targptr%center = location
                targptr%normal = normal

                targptr%depth_loc = temperature_depth

                ! target name
                targptr%name = id

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
                    write (*,913) 'Error',type
                    write (iofill,913) 'Error',type
                    stop
                end if

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
                    write (*,'(a,a)') '***Error in &DEVC: Bad type. Not known for ', type
                    write (iofill,'(a,a)') '***Error in &DEVC: Bad type. Not known for ', type
                end if

                dtectptr%dtype = i1

                iroom = 0
                compartment_id = ' '
                compartment_id = trim(comp_id)

                idcheck=.false.
                searching_2: do jj = 1, nr-1
                    roomptr => roominfo(jj)
                    if (trim(compartment_id) == trim(roomptr%name)) then
                        iroom = roomptr%compartment
                        idcheck = .true.
                        exit searching_2
                    end if
                end do searching_2

                if (.not. idcheck) then
                    write (*,'(a,a,a,i3)') '***Error in &DEVC: COMP_ID: ', id, ', not found. Check detector, ', counter2
                    write (iofill,'(a,a,a,i3)') '***Error in &DEVC: COMP_ID: ', id, ', not found. Check detector, ', counter2
                    stop
                end if

                dtectptr%room = iroom
                if (iroom<1.or.iroom>mxrooms) then
                    write (*,5342) iroom
                    write (iofill,5342) iroom
                    stop
                end if

                dtectptr%name = id
                if (trim(type) == 'SPRINKLER' .or. trim(type) == 'HEAT_DETECTOR') then
                    if (setpoint/=-101._eb) then
                        dtectptr%trigger = setpoint + 273.15_eb
                    else
                        dtectptr%trigger = default_activation_temperature
                    end if
                else
                    if (setpoint/=-101._eb) then
                        dtectptr%trigger = setpoint
                    else
                        dtectptr%trigger = default_activation_obscuration
                    end if
                end if
                dtectptr%center = location
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
                write (*,5339) n_detectors,roomptr%name
                write (iofill,5339) n_detectors,roomptr%name
                stop
                end if

            end if

        end do read_devc_loop

    end if devc_detec_flag

913 format('***',A,': BAD DEVC input. Invalid equation type:',A3,' Valid choices are: PDE or CYL')
5003 format ('***Error: BAD DEVC input. The compartment specified by DEVC does not exist ',i0)

5339 format ('***Error: Bad DETEC input. Detector ',i0,' is outside of compartment ',a)
5342 format ('***Error: Bad DETEC input. Invalid DETECTOR specification - room ',i0)



    contains

    subroutine set_devc_defaults

    comp_id                         = 'NULL'
    type                            = 'NULL'
    id                              = 'NULL'
    temperature_depth               = 0.5_eb
    location(:)                     = (/-1.0_eb, -1.0_eb, -3.0_eb/39.37_eb/)
    matl_id                         = 'NULL'
    normal(:)                       = (/0., 0., 1./)
    rti                             = default_rti
    setpoint                        = -101._eb
    spray_density                   = -300.0_eb

    end subroutine set_devc_defaults

    end subroutine read_devc


    ! --------------------------- RAMP -------------------------------------------
    subroutine read_ramp(lu)

    integer, intent(in) :: lu
    
    integer :: ii, ios

    type(ramp_type), pointer :: rampptr

    real(eb), dimension(mxpts) :: f, t, z
    character(64) :: type,id
    character(64), dimension(2) :: comp_ids
    namelist /RAMP/ f, id ,t, z, type, comp_ids

    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0

    ! Scan entire file to look for 'RAMP'
    nramps = 0
    ramp_loop: do
        call checkread ('RAMP',lu,ios)
        if (ios==0) rampflag=.true.
        if (ios==1) then
            exit ramp_loop
        end if
        read(lu,RAMP,iostat=ios)
        nramps =nramps + 1
        if (ios>0) then
            write(iofill, '(a,i3)') '***Error in &RAMP: Invalid specification for inputs. Check &RAMP input, ', nramps
            stop
        end if
    end do ramp_loop

    if (nramps>mxramps) then
        write (*,'(a,i3)') '***Error: Too many ramps in input data file. Limit is ', mxramps
        write (iofill,'(a,i3)') '***Error: Too many ramps in input data file. Limit is ', mxramps
        stop
    end if

    ramp_flag: if (rampflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_ramp_loop: do ii = 1,nramps

            call checkread('RAMP',lu,ios)
            call set_ramp_defaults
            read(lu,RAMP)

            rampptr => rampinfo(ii)
            rampptr%id = id
            if (count(z/=-101._eb)>0 .and. count(t/=-101._eb)>0) then
                write (*,'(a,i3)') '***Error in &RAMP: Cannot use both z and t in a ramp. Check ramp, ', nramps
                write (iofill,'(a,i3)') '***Error in &RAMP: Cannot use both z and t in a ramp. Check ramp, ', nramps
            else if (count(z/=-101._eb)==0 .and. count(t/=-101._eb)==0) then
                write (*,'(a,i3)') '***Error in &RAMP: Either z or t must be in a ramp. Check ramp, ', nramps
                write (iofill,'(a,i3)') '***Error in &RAMP: Either z or t must be in a ramp. Check ramp, ', nramps
            end if
            
            if (type=='AREA' .and. count(z/=-101._eb)>0) then
                rampptr%x(1:mxpts)  = z(1:mxpts)
            else
                rampptr%x(1:mxpts) = t(1:mxpts)
            end if
            rampptr%f_of_x(1:mxpts) = f(1:mxpts)

            if (count(rampptr%x/=-101._eb) /= count(rampptr%f_of_x/=-101._eb)) then
                if (type=='AREA') then
                    write (*,'(a,i3)') '***Error in &RAMP: The number of inputs for z and f do not match. Check ramp, ', nramps
                    write (iofill,'(a,i3)') '***Error in &RAMP: The number of inputs for z and f do not match. Check ramp, ', nramps
                else
                    write (*,'(a,i3)') '***Error in &RAMP: The number of inputs for t and f do not match. Check ramp, ', nramps
                    write (iofill,'(a,i3)') '***Error in &RAMP: The number of inputs for t and f do not match. Check ramp, ', nramps
                end if
                stop
            end if
            rampptr%npoints=count(rampptr%x/=-101._eb)

        end do read_ramp_loop

    end if ramp_flag


    contains

    subroutine set_ramp_defaults

    type                    = 'NULL'
    t(:)                    = -101._eb
    f(:)                    = -101._eb
    z(:)                    = -101._eb
    id                      = 'NULL'

    end subroutine set_ramp_defaults

    end subroutine read_ramp


    ! --------------------------- FIRE -------------------------------------------
    subroutine read_fire(lu)

    integer, intent(in) :: lu
    
    integer :: ios, i, ii, jj, kk, iroom, base, midpoint
    real(eb) :: tmpcond, max_hrr, flamelength, hrrpm3, max_area, ohcomb
    character(64) :: compartment_id

    type(room_type),   pointer :: roomptr
    type(fire_type),   pointer :: fireptr
    type(target_type), pointer :: targptr
    type(ramp_type),   pointer :: rampptr

    real(eb) :: carbon, chlorine, hydrogen, nitrogen, oxygen
    real(eb) :: area, co_yield, hcl_yield, hcn_yield, heat_of_combustion, hrr, radiative_fraction, setpoint, &
        soot_yield, trace_yield
    real(eb), dimension(3) :: location
    character(64) :: area_ramp_id, co_yield_ramp_id, comp_id, devc_id, id, hcl_yield_ramp_id, hcn_yield_ramp_id, hrr_ramp_id, &
        ignition_criterion, soot_yield_ramp_id, trace_yield_ramp_id
    namelist /FIRE/ area, area_ramp_id, carbon, chlorine, comp_id, co_yield, co_yield_ramp_id, devc_id, heat_of_combustion, &
        hcl_yield, hcn_yield, hcl_yield_ramp_id, hcn_yield_ramp_id, hrr, hrr_ramp_id, hydrogen, id, ignition_criterion, location, &
        nitrogen, oxygen, radiative_fraction, setpoint, soot_yield, soot_yield_ramp_id, trace_yield, trace_yield_ramp_id

    ios = 1
    tmpcond = 0.0

    rewind (unit=lu)
    input_file_line_number = 0

    ! Scan entire file to look for 'FIRE'
    n_fires = 0
    fire_loop: do
        call checkread ('FIRE', lu, ios)
        if (ios==0) fireflag = .true.
        if (ios==1) then
            exit fire_loop
        end if
        read(lu,FIRE,iostat=ios)
        if (ios>0) then
            write(iofill, '(a,i3)') '***Error in &FIRE: Invalid specification for inputs. Check &FIRE input, ', n_fires+1
            stop
        end if
        n_fires =n_fires + 1
    end do fire_loop

    if (n_fires>mxfires) then
        write (*,'(a,i3)') '***Error: Too many fires in input data file. Limit is ', mxfires
        write (iofill,'(a,i3)') '***Error: Too many fires in input data file. Limit is ', mxfires
        stop
    end if

    fire_flag: if (fireflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_fire_loop: do ii = 1, n_fires

            fireptr => fireinfo(ii)

            call checkread('FIRE',lu,ios)
            call set_fire_defaults
            read(lu,FIRE)

            iroom = 0
            compartment_id = ' '
            compartment_id = trim(comp_id)

            searching: do jj = 1, nr-1
                roomptr => roominfo(jj)
                if (trim(compartment_id) == trim(roomptr%name)) then
                    iroom = roomptr%compartment
                    exit searching
                end if
            end do searching

            if (iroom<1.or.iroom>nr-1) then
                write (*,5320) iroom
                write (iofill,5320) iroom
                stop
            end if
            roomptr => roominfo(iroom)

            fireptr%room = iroom
            fireptr%name = id

            ! Only constrained fires
            fireptr%chemistry_type = 2
            if (fireptr%chemistry_type>2) then
                write (*,5321) fireptr%chemistry_type
                write (iofill,5321) fireptr%chemistry_type
                stop
            end if

            fireptr%x_position = location(1)
            fireptr%y_position = location(2)
            fireptr%z_position = location(3)
            if (fireptr%x_position>roomptr%cwidth.or.fireptr%y_position>roomptr%cdepth.or.fireptr%z_position>roomptr%cheight) then
                write (*,5323) n_fires
                write (iofill,5323) n_fires
                stop
            end if

            fireptr%modified_plume = 1
            if (min(fireptr%x_position,roomptr%cwidth-fireptr%x_position)<=mx_hsep .or. &
                min(fireptr%y_position,roomptr%cdepth-fireptr%y_position)<=mx_hsep) fireptr%modified_plume = 2
            if (min(fireptr%x_position,roomptr%cwidth-fireptr%x_position)<=mx_hsep .and. &
                min(fireptr%y_position,roomptr%cdepth-fireptr%y_position)<=mx_hsep) fireptr%modified_plume = 3

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
                            if (trim(targptr%name)==trim(devc_id)) fireptr%ignition_target = i
                        end do
                        if (fireptr%ignition_target==0) then
                            write (*,5324) n_fires
                            write (iofill,5324) n_fires
                            stop
                        end if
                    end if
                else
                    write (*,5322)
                    write (iofill,5322)
                    stop
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
                    fireptr%ignition_criterion = tmpcond + 273.15
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
                    write (*,5358) fireptr%ignition_type
                    write (iofill,5358) fireptr%ignition_type
                    stop
                end if
            else
                fireptr%ignited  = .true.
                fireptr%reported = .true.
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
            ohcomb = heat_of_combustion *1.e3_eb
            if (ohcomb<=0.0_eb) then
                write (*,5001) ohcomb
                write (iofill,5001) ohcomb
                stop
            end if
            
            ! do constant values for fire inputs first, then check for ramps

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

            ! constant trace species
            fireptr%n_trace = 1
            fireptr%t_trace(1) = 0.0_eb
            fireptr%y_trace(1) = trace_yield

            ! constant area
            fireptr%n_area = 1
            fireptr%t_area(1) = 0.0_eb
            fireptr%area(1) = max(area,pio4*0.2_eb**2)

            ! height is no longer variable. it's input is through location(3) and fixed
            fireptr%n_height = 1
            fireptr%t_height = 0.0_eb
            fireptr%height(1) = 0.0_eb

            ramp_search: do kk = 1, nramps
                rampptr=>rampinfo(kk)

                ! Define hrr
                if (trim(rampptr%id) == trim(hrr_ramp_id)) then
                    rampptr%room1 = iroom
                    rampptr%room2 = iroom
                    rampptr%counter = kk
                    fireptr%n_qdot = rampptr%npoints
                    fireptr%t_qdot = rampptr%x
                    fireptr%qdot = rampptr%f_of_x * 1000._eb
                end if

                max_hrr = 0.0_eb
                do i = 1, fireptr%n_qdot
                    max_hrr = max(max_hrr, fireptr%qdot(i))
                end do

                ! Define soot
                if (trim(rampptr%id) == trim(soot_yield_ramp_id)) then
                    rampptr%room1 = iroom
                    rampptr%room2 = iroom
                    rampptr%counter = kk
                    fireptr%n_soot = rampptr%npoints
                    fireptr%t_soot = rampptr%x
                    fireptr%y_soot = rampptr%f_of_x
                end if

                ! define co
                if (trim(rampptr%id) == trim(co_yield_ramp_id)) then
                    rampptr%room1 = iroom
                    rampptr%room2 = iroom
                    rampptr%counter = kk
                    fireptr%n_co = rampptr%npoints
                    fireptr%t_co = rampptr%x
                    fireptr%y_co = rampptr%f_of_x
                end if

                ! define trace
                ! note that ct, tuhc and ts are carried in the mprodr array - all other species have their own array
                if (trim(rampptr%id) == trim(trace_yield_ramp_id)) then
                    rampptr%room1 = iroom
                    rampptr%room2 = iroom
                    rampptr%counter = kk
                    fireptr%n_co = rampptr%npoints
                    fireptr%t_trace = rampptr%x
                    fireptr%y_trace = rampptr%f_of_x
                end if

                ! define area

                ! the minimum area is to stop dassl from a floating point underflow when it tries to extrapolate back to the
                ! ignition point. it only occurs for objects which are on the floor and ignite after t=0. the assumed minimum fire
                ! diameter of 0.2 m below is the minimum valid fire diameter for heskestad's plume correlation
                ! (from sfpe handbook chapter)
                if (trim(rampptr%id) == trim(area_ramp_id)) then
                    rampptr%room1 = iroom
                    rampptr%room2 = iroom
                    rampptr%counter = kk
                    fireptr%n_area = rampptr%npoints
                    fireptr%t_area = rampptr%x
                    fireptr%area = rampptr%f_of_x
                end if

            end do ramp_search

            ! calculate mass loos rate from hrr and hoc inputs
            fireptr%mdot = fireptr%qdot / ohcomb
            fireptr%t_mdot = fireptr%t_qdot
            fireptr%n_mdot = fireptr%n_qdot
            ! set the heat of combustion - this is a problem if the qdot is zero and the mdot is zero as well
            call set_heat_of_combustion (fireptr%n_qdot, fireptr%mdot, fireptr%qdot, fireptr%hoc, ohcomb)
            fireptr%t_hoc = fireptr%t_qdot
            fireptr%n_hoc = fireptr%n_qdot

            ! maximum area, used for input check of hrr per flame volume
            max_area = 0.0_eb
            do i = 1, fireptr%n_area
                max_area = max(max_area,max(fireptr%area(i),pio4*0.2_eb**2))
            end do
            if (max_area==0.0_eb) then
                write (*,5002)
                write (iofill,5002)
                stop
            end if

            ! calculate a characteristic length of an object (we assume the diameter).
            ! this is used for point source radiation fire to target calculation as a minimum effective
            ! distance between the fire and the target which only impact very small fire to target distances
            fireptr%characteristic_length = sqrt(max_area/pio4)

            ! Position the object
            roomptr => roominfo(fireptr%room)
            call position_object (fireptr%x_position,roomptr%cwidth,midpoint,mx_hsep)
            call position_object (fireptr%y_position,roomptr%cdepth,midpoint,mx_hsep)
            call position_object (fireptr%z_position,roomptr%cheight,base,mx_hsep)

            ! Diagnostic - check for the maximum heat release per unit volume.
            ! First, estimate the flame length - we want to get an idea of the size of the volume over which the energy will be released
            call flame_height(max_hrr, max_area, flamelength)
            flamelength = max (0.0_eb, flamelength)

            ! Now the heat release per cubic meter of the flame - we know that the size is larger than 1.0d-6 m^3 - enforced above
            hrrpm3 = max_hrr/(pio4*fireptr%characteristic_length**2*(fireptr%characteristic_length+flamelength))
            if (hrrpm3>4.0e6_eb) then
                write (*,5106) trim(fireptr%name),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
                write (*, 5108)
                write (iofill,5106) trim(fireptr%name),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
                write (iofill, 5108)
                stop
            else if (hrrpm3>2.0e6_eb) then
                write (*,5107) trim(fireptr%name),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
                write (*, 5108)
                write (iofill,5107) trim(fireptr%name),fireptr%x_position,fireptr%y_position,fireptr%z_position,hrrpm3
                write (iofill, 5108)
            end if

        end do read_fire_loop

    end if fire_flag

5320 format ('***Error: Bad FIRE input. Fire specification error, room ',i0,' out of range')
5321 format ('***Error: Bad FIRE input. Fire specification error, not an allowed fire type',i0)
5322 format ('***Error: Bad FIRE input. Fire specification is outdated and must include target for ignition')
5323 format ('***Error: Bad FIRE input. Fire location ',i0,' is outside its compartment')
5324 format ('***Error: Bad FIRE input. Target specified for fire ',i0, ' does not exist')
5358 format ('***Error: Bad FIRE input. Not a valid ignition criterion ',i0)

5001 format ('***Error: invalid heat of combustion, must be greater than zero, ',1pg12.3)
5002 format ('***Error: invalid fire area. all input values must be greater than zero')
5106 format ('***Error: object ',a,' position set to ',3f7.3,'; maximum hrr per m^3 = ',1pg10.3,' exceeds physical limits')
5107 format ('Object ',a,' position set to ',3f7.3,'; maximum c_hrr per m^3 = ',1pg10.3,' exceeds nominal limits')
5108 format ('Typically, this is caused by too small fire area inputs. check hrr and fire area inputs')
5000 format ('***Error: the key word ',a5,' is not part of a fire definition. fire keywords are likely out of order')

    contains

    subroutine set_fire_defaults

    area                      = 0._eb
    area_ramp_id              = 'NULL'
    carbon                    = 0._eb
    chlorine                  = 0._eb
    comp_id                   = 'NULL'
    co_yield                  = 0._eb
    co_yield_ramp_id           = 'NULL'
    devc_id                   = 'NULL'
    hcn_yield                 = 0.0_eb
    hcn_yield_ramp_id         = 'NULL'
    heat_of_combustion        = 50000._eb
    hrr                       = 0.0_eb
    hrr_ramp_id               = 'NULL'
    hydrogen                  = 0._eb
    id                        = 'NULL'
    ignition_criterion        = 'TIME'
    location(:)               = 0._eb
    nitrogen                  = 0._eb
    oxygen                    = 0._eb
    radiative_fraction        = 0._eb
    setpoint                  = 0._eb
    soot_yield                = 0._eb
    soot_yield_ramp_id        = 'NULL'
    trace_yield               = 0._eb
    trace_yield_ramp_id       = 'NULL'

    end subroutine set_fire_defaults

    end subroutine read_fire


    ! --------------------------- VENT -------------------------------------------
    subroutine read_vent(lu)

    integer, intent(in) :: lu

    integer :: i, ii, j, jj, k, kk, mm, imin, jmax, counter1, counter2, counter3, iroom
    integer :: ios
    character(64) :: compartment_id
    real(eb) :: initialtime, initialfraction, finaltime, finalfraction

    type(room_type), pointer :: roomptr
    type(target_type), pointer :: targptr
    type(vent_type), pointer :: ventptr
    type(ramp_type), pointer :: rampptr

    real(eb) :: area, bottom, flow, offset, setpoint, top, width, pre_fraction, post_fraction, filter_time, filter_efficiency
    real(eb),dimension(2):: areas, cutoffs, heights, offsets
    character(64),dimension(2) :: comp_ids, orientations
    character(64) :: criterion, devc_id, face, filtering_ramp_id, id, opening_ramp_id, shape, type
    namelist /VENT/ area, areas, bottom, comp_ids, criterion, cutoffs, devc_id, face, filter_efficiency, filtering_ramp_id, &
        filter_time, flow, heights, id, offset, offsets, opening_ramp_id, orientations, pre_fraction, post_fraction, &
        setpoint, shape, top, type, width

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
            write(*, '(3a,i0)') '***Error in &VENT: Invalid specification for inputs. Check &VENT input, ',trim(id),': ', &
                n_hvents + n_mvents + n_vvents
            write(iofill, '(3a,i0)') '***Error in &VENT: Invalid specification for inputs. Check &VENT input, ',trim(id),': ', &
                n_hvents + n_mvents + n_vvents
            stop
        end if
    end do vent_loop

    if (n_hvents>mxhvents) then
        write (*,'(a,i3)') '***Error: Too many wall vents in input data file. Limit is ', mxhvents
        write (iofill,'(a,i3)') '***Error: Too many wall vents in input data file. Limit is ', mxhvents
        stop
    end if

    if (n_mvents>mxmvents) then
        write (*,'(a,i3)') '***Error: Too many mechanical vents in input data file. Limit is ', mxmvents
        write (iofill,'(a,i3)') '***Error: Too many mechanical vents in input data file. Limit is ', mxmvents
        stop
    end if

    if (n_vvents>mxvvents) then
        write (*,'(a,i3)') '***Error: Too many celing/floor vents in input data file. Limit is ', mxvvents
        write (iofill,'(a,i3)') '***Error: Too many ceiling/floor vents in input data file. Limit is ', mxvvents
        stop
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
            call set_vent_defaults
            read(lu,VENT)

            ! Wall vent
            if (trim(type) == 'WALL') then
                counter1=counter1+1

                i=0
                j=0

                do mm = 1, 2
                    iroom=-101
                    compartment_id=' '
                    compartment_id=trim(comp_ids(mm))

                    searching: do jj=1,nr-1
                        roomptr => roominfo(jj)
                        if (trim(compartment_id) == 'OUTSIDE') then
                            iroom = nr
                            exit searching
                        end if
                        if (trim(compartment_id) == trim(roomptr%name)) then
                            iroom = roomptr%compartment
                            exit searching
                        end if
                    end do searching

                    if (iroom == -101) then
                        write (*,'(a,a)') '***Error: COMP_IDS do not specify existing compartments. ', comp_ids(mm)
                        write (iofill,'(a,a)') '***Error: COMP_IDS do not specify existing compartments. ', comp_ids(mm)
                        stop
                    end if

                    if (mm == 1) i = iroom
                    if (mm == 2) j = iroom
                end do

                imin = min(i,j)
                jmax = max(i,j)

                if (imin>mxrooms-1.or.jmax>mxrooms.or.imin==jmax) then
                    write (*,5070) i, j
                    write (iofill,5070) i, j
                    stop
                end if
                if (counter1>mxperrm) then
                    write (*,5080) i, j, k
                    write (iofill,5080) i, j, k
                    stop
                end if

                ventptr => hventinfo(counter1)
                ventptr%room1 = i
                ventptr%room2 = j
                ventptr%counter = counter1
                ventptr%ramp_id = opening_ramp_id

                if (n_hvents>mxhvents) then
                    write (*,5081) i,j,k
                    write (iofill,5081) i,j,k
                    stop
                end if

                ventptr%width  = width
                ventptr%soffit = top
                ventptr%sill   = bottom

                if  (trim(criterion)=='TIME' .or. trim(criterion)=='TEMPERATURE' .or. trim(criterion)=='FLUX') then
                    ventptr%offset(1) = offset
                    ventptr%offset(2) = 0

                    if (trim(face) == 'FRONT') ventptr%face=1
                    if (trim(face) == 'RIGHT') ventptr%face=2
                    if (trim(face) == 'BACK') ventptr%face=3
                    if (trim(face) == 'LEFT') ventptr%face=4

                    initialtime = 0._eb
                    initialfraction = pre_fraction
                    finaltime = 0._eb
                    finalfraction = post_fraction

                    if (trim(opening_ramp_id) /= 'NULL') then
                        ramp_search: do kk=1,nramps
                            rampptr=>rampinfo(kk)
                            if (trim(rampptr%id) == trim(opening_ramp_id)) then
                                rampptr%room1 = i
                                rampptr%room2 = j
                                rampptr%counter = kk
                                exit ramp_search
                            end if
                        end do ramp_search

                        if (kk == nramps+1) then
                            write (*,'(a,a)') '***Error: RAMP ID cannot be found in input file. ', opening_ramp_id
                            write (iofill,'(a,a)') '***Error: RAMP ID cannot be found in input file. ', opening_ramp_id
                            stop
                        end if
                    end if

                    if (trim(criterion)=='TIME') then
                        ventptr%opening_type = trigger_by_time
                        ventptr%opening_initial_time = initialtime
                        ventptr%opening_initial_fraction = initialfraction
                        ventptr%opening_final_time = finaltime
                        ventptr%opening_final_fraction = finalfraction
                    else
                        if (trim(criterion)=='TEMPERATURE') then
                            ventptr%opening_type = trigger_by_temp
                            ventptr%opening_criterion = setpoint + 273.15
                        end if
                        if (criterion=='FLUX') then
                            ventptr%opening_type = trigger_by_flux
                            ventptr%opening_criterion = setpoint * 1000._eb
                        end if
                        ventptr%opening_target = 0
                        do i = 1,n_targets
                            targptr => targetinfo(i)
                            if (trim(targptr%name)==trim(devc_id)) ventptr%opening_target = i
                        end do
                        if (ventptr%opening_target==0) then
                            write (*,*) '***Error: Vent opening specification requires an associated target.'
                            write (iofill,*) '***Error: Vent opening specification requires an associated target.'
                            stop
                        end if
                        ventptr%opening_initial_fraction = initialfraction
                        ventptr%opening_final_fraction = finalfraction
                        if (stpmax>0) then
                            stpmax = min(stpmax,1.0_eb)
                        else
                            stpmax = 1.0_eb
                        end if
                    end if
                else
                    write (*,*) '***Error: Inputs for wall vent: criterion has to be "TIME", "TEMPERATURE", or "FLUX".'
                    write (iofill,*) '***Error: Inputs for wall vent: criterion has to be "TIME", "TEMPERATURE", or "FLUX".'
                    stop
                end if

                ! Avoiding referring "OUTSIDE"
                if (j /= nr) then
                    roomptr => roominfo(j)
                else
                    roomptr => roominfo(i)
                end if
                ventptr%absolute_soffit = ventptr%soffit + roomptr%z0
                ventptr%absolute_sill = ventptr%sill + roomptr%z0

                ! Mechanical vent
            else if (trim(type) == 'MECHANICAL') then
                counter2=counter2+1

                i=0
                j=0

                do mm = 1, 2
                    iroom=-101
                    compartment_id=' '
                    compartment_id=trim(comp_ids(mm))

                    searching_2: do jj=1,nr-1
                        roomptr => roominfo(jj)
                        if (trim(compartment_id) == 'OUTSIDE') then
                            iroom = nr
                            exit searching_2
                        end if
                        if (trim(compartment_id) == trim(roomptr%name)) then
                            iroom = roomptr%compartment
                            exit searching_2
                        end if
                    end do searching_2

                    if (iroom == -101) then
                        write (*,'(a,a)') '***Error: COMP_IDS do not specify existing compartments. ', comp_ids(mm)
                        write (iofill,'(a,a)') '***Error: COMP_IDS do not specify existing compartments. ', comp_ids(mm)
                        stop
                    end if

                    if (mm == 1) i=iroom
                    if (mm == 2) j=iroom
                end do

                k = counter2
                if (i>nr.or.j>nr) then
                    write (*,5191) i, j
                    write (iofill,5191) i, j
                    stop
                end if

                ventptr => mventinfo(counter2)
                ventptr%room1 = i
                ventptr%room2 = j
                ventptr%counter = counter2
                ventptr%ramp_id = opening_ramp_id
                ventptr%filter_id = filtering_ramp_id
                ventptr%filter_initial_time = filter_time
                ventptr%filter_final_time = filter_time + 1.0_eb
                ventptr%filter_final_fraction = filter_efficiency / 100.0_eb

                ventptr%orientation(1) = 1
                ventptr%orientation(2) = 1

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

                if (trim(criterion) /= 'NULL') then
                    if (trim(criterion)=='TIME' .or. trim(criterion)=='TEMPERATURE' .or. trim(criterion)=='FLUX') then

                        initialtime = 0._eb       ! in namelist input, these are just placeholders for the older event data
                        initialfraction = pre_fraction
                        finaltime = 0._eb
                        finalfraction = post_fraction

                        if (trim(opening_ramp_id) /= 'NULL') then
                            ramp_search_2: do kk=1,nramps
                                rampptr => rampinfo(kk)
                                if (trim(rampptr%id) == trim(opening_ramp_id)) then
                                    rampptr%room1 = i
                                    rampptr%room2 = j
                                    rampptr%counter = kk
                                    exit ramp_search_2
                                end if
                            end do ramp_search_2

                            if (kk == nramps+1) then
                                write (*,'(a,a)') '***Error: RAMP ID cannot be found in input file. ', opening_ramp_id
                                write (iofill,'(a,a)') '***Error: RAMP ID cannot be found in input file. ', opening_ramp_id
                                stop
                            end if
                        end if

                        if (trim(criterion)=='TIME') then
                            ventptr%opening_type = trigger_by_time
                            ventptr%opening_initial_time = initialtime
                            ventptr%opening_initial_fraction = initialfraction
                            ventptr%opening_final_time = finaltime
                            ventptr%opening_final_fraction = finalfraction
                        else
                            if (trim(criterion)=='TEMPERAUTRE') ventptr%opening_type = trigger_by_temp
                            if (trim(criterion)=='FLUX') ventptr%opening_type = trigger_by_flux
                            ventptr%opening_criterion = setpoint
                            ventptr%opening_target = 0
                            do i = 1,n_targets
                                targptr => targetinfo(i)
                                if (trim(targptr%name)==trim(devc_id)) ventptr%opening_target = i
                            end do
                            if (ventptr%opening_target==0) then
                                write (*,*) '***Error: Vent opening specification requires an associated target.'
                                write (iofill,*) '***Error: Vent opening specification requires an associated target.'
                                stop
                            end if
                            ventptr%opening_initial_fraction = initialfraction
                            ventptr%opening_final_fraction = finalfraction
                        end if
                        ventptr%xoffset = offsets(1)
                        ventptr%yoffset = offsets(2)
                        if (stpmax>0) then
                            stpmax = min(stpmax,1.0_eb)
                        else
                            stpmax = 1.0_eb
                        end if
                    else
                        write (*,*) 'Inputs for mechanical vent: criterion has to be "TIME", "TEMPERATURE", or "FLUX".'
                        write (iofill,*) 'Inputs for mechanical vent: criterion has to be "TIME", "TEMPERATURE", or "FLUX".'
                        stop
                    end if
                end if

                ! Ceiling/Floor vents
            else if (trim(type) == 'CEILING' .or. trim(type) == 'FLOOR') then
                counter3=counter3+1

                i=0
                j=0

                do mm = 1, 2
                    iroom = -101
                    compartment_id = ' '
                    compartment_id = trim(comp_ids(mm))

                    searching_3: do jj=1,nr-1
                        roomptr => roominfo(jj)
                        if (trim(compartment_id) == 'OUTSIDE') then
                            iroom = nr
                            exit searching_3
                        end if
                        if (trim(compartment_id) == trim(roomptr%name)) then
                            iroom = roomptr%compartment
                            exit searching_3
                        end if
                    end do searching_3

                    if (iroom == -101) then
                        write (*,'(a,a)') '***Error: COMP_IDS do not specify existing compartments. ', comp_ids(mm)
                        write (iofill,'(a,a)') '***Error: COMP_IDS do specify existing compartments. ', comp_ids(mm)
                        stop
                    end if

                    if (mm == 1) i = iroom
                    if (mm == 2) j = iroom
                end do

                k = counter3

                ! check for outside of compartment space; self pointers are covered in read_input_file
                if (i>mxrooms.or.j>mxrooms) then
                    write (*,5070) i, j
                    write (iofill,5070) i, j
                    stop
                end if

                ventptr => vventinfo(counter3)
                ventptr%room1 = i
                ventptr%room2 = j
                ventptr%counter = counter3
                ventptr%ramp_id = opening_ramp_id

                ! read_input_file will verify the orientation (i is on top of j)
                ventptr%area = area

                ! check the shape parameter. the default (1) is a circle)
                if (trim(shape) == 'ROUND') then
                    ventptr%shape = 1
                else if (trim(shape) == 'SQUARE') then
                    ventptr%shape = 2
                else
                    write (*,'(a,a)') '***Error: SHAPE must be SQUARE or ROUND. ', shape
                    write (iofill,'(a,a)') '***Error: SHAPE must be SQUARE or ROUND. ', shape
                end if

                if (trim(criterion) /='NULL') then
                    if (trim(criterion)=='TIME' .or. trim(criterion)=='TEMPERATURE' .or. trim(criterion)=='FLUX') then

                        initialtime = 0._eb
                        initialfraction = 1._eb
                        finaltime = 0._eb
                        finalfraction = 1._eb

                        if (trim(opening_ramp_id) /= 'NULL') then
                            ramp_search_3: do kk=1,nramps
                                rampptr=>rampinfo(kk)
                                if (trim(rampptr%id) == trim(opening_ramp_id)) then
                                    rampptr%room1 = i
                                    rampptr%room2 = j
                                    rampptr%counter = kk
                                    exit ramp_search_3
                                end if
                            end do ramp_search_3

                            if (kk == nramps+1) then
                                write (*,'(a,a)') '***Error: RAMP ID cannot be found in input file. ', opening_ramp_id
                                write (iofill,'(a,a)') '***Error: RAMP ID cannot be found in input file. ', opening_ramp_id
                                stop
                            end if
                        end if

                        if (trim(criterion)=='TIME') then
                            ventptr%opening_type = trigger_by_time
                            ventptr%opening_initial_time = initialtime
                            ventptr%opening_initial_fraction = initialfraction
                            ventptr%opening_final_time = finaltime
                            ventptr%opening_final_fraction = finalfraction
                        else
                            if (trim(criterion)=='TEMPERATURE') ventptr%opening_type = trigger_by_temp
                            if (trim(criterion)=='FLUX') ventptr%opening_type = trigger_by_flux
                            ventptr%opening_criterion = setpoint
                            ventptr%opening_target = 0
                            do i = 1,n_targets
                                targptr => targetinfo(i)
                                if (trim(targptr%name)==trim(devc_id)) ventptr%opening_target = i
                            end do
                            if (ventptr%opening_target==0) then
                                write (*,*) '***Error: Vent opening specification requires an associated target.'
                                write (iofill,*) '***Error: Vent opening specification requires an associated target.'
                                stop
                            end if
                            ventptr%opening_initial_fraction = initialfraction
                            ventptr%opening_final_fraction = finalfraction
                            if (stpmax>0) then
                                stpmax = min(stpmax,1.0_eb)
                            else
                                stpmax = 1.0_eb
                            end if
                        end if
                        ventptr%xoffset = offsets(1)
                        ventptr%yoffset = offsets(2)
                    else
                        write (*,*) 'Inputs for ceiling/floor vent: criterion has to be "TIME", "TEMPERATURE", or "FLUX".'
                        write (iofill,*) 'Inputs for ceiling/floor vent: criterion has to be "TIME", "TEMPERATURE", or "FLUX".'
                        stop
                    end if

                end if
            end if

        end do read_vent_loop

    end if vent_flag

5070 format ('***Error: Bad VENT input. Parameter(s) outside of allowable range',2I4)
5080 format ('***Error: Bad VENT input. Too many pairwise horizontal connections',3I5)
5081 format ('***Error: Too many horizontal connections ',3i5)

5191 format ('***Error: Bad MVENT input. Compartments specified in MVENT have not been defined ',2i3)

    contains

    subroutine set_vent_defaults

    area                  = 0._eb
    areas(:)              = 0._eb
    bottom                = 0._eb
    comp_ids(:)           = 'NULL'
    criterion             = 'TIME'
    cutoffs(:)            = 0._eb
    devc_id               = 'NULL'
    face                  = 'NULL'
    filter_time           = 0._eb
    filter_efficiency     = 0._eb
    filtering_ramp_id     = 'NULL'
    flow                  = 0._eb
    heights(:)            = 0._eb
    id                    = 'NULL'
    offset                = 0._eb
    offsets(:)            = 0._eb
    opening_ramp_id       = 'NULL'
    orientations          = 'VERTICAL'
    pre_fraction          = 1._eb
    post_fraction         = 1._eb
    setpoint              = 0._eb
    shape                 = 'NULL'
    top                   = 0._eb
    type                  = 'NULL'
    width                 = 0._eb

    end subroutine set_vent_defaults

    end subroutine read_vent


    ! --------------------------- CONN -------------------------------------------
    subroutine read_conn(lu)

    integer, intent(in) :: lu

    integer :: ios, ifrom, ito, i, k, jj, i1, i2, counter1
    real(eb), dimension(mxpts) :: frac
    character(64) :: compartment_id
    integer :: nmlcount                             ! count of number of each namelist type read in so far

    type(room_type), pointer :: roomptr

    real(eb), dimension(mxpts) :: f
    character(64), dimension(mxpts) :: comp_ids
    character(64) :: comp_id, type
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
        if (trim(type) == trim('CEILING') .or. trim(type) == trim('FLOOR')) nvcons = nvcons + 1
        if (trim(type) == trim('WALL')) nmlcount  =nmlcount + 1
34      if (ios>0) then
            write(iofill, '(a,i3)') 'Error: Invalid specification in &CONN inputs. Check &CONN input, ' , nvcons+nmlcount
            stop
        end if
    end do conn_loop

    conn_flag: if (connflag) then

        rewind (lu)
        input_file_line_number = 0

        counter1 = 0

        countloop : do k = 1, nmlcount + nvcons

            call checkread('CONN',lu,ios)
            call set_conn_defaults
            read(lu,CONN)

            if (trim(type) == 'WALL') then
                frac(:)=-101
                compartment_id = ' '
                compartment_id = comp_id
                ifrom = -101

                searching: do jj=1,nr-1
                    roomptr => roominfo(jj)
                    if (trim(compartment_id) == trim(roomptr%name)) then
                        ifrom = roomptr%compartment
                        exit searching
                    end if
                end do searching

                if (ifrom == -101) then
                    write (*,'(a,a)') '***Error: COMP_ID not found. ', comp_id
                    write (iofill,'(a,a)') '***Error: COMP_ID not found. ', comp_id
                    stop
                end if

                roomptr => roominfo(ifrom)
                roomptr%iheat = 2

                frac(:) = f(:)

                do i = 1, count(frac /= -101._eb)
                    compartment_id = ' '
                    compartment_id = comp_ids(i)
                    ito=-101

                   searching_2: do jj=1,nr-1
                        roomptr => roominfo(jj)
                        if (trim(compartment_id) == 'OUTSIDE') then
                            ito = nr
                            exit searching_2
                        end if
                        if (trim(compartment_id) == trim(roomptr%name)) then
                            ito = roomptr%compartment
                            exit searching_2
                        end if
                    end do searching_2

                    if (ito == -101) then
                        write (*,'(a,a)') '***Error: COMP_IDS do not match existing compartments. ', comp_ids(i)
                        write (iofill,'(a,a)') '***Error: COMP_IDS do not match existing compartments. ', comp_ids(i)
                        stop
                    end if

                    if (ito<1.or.ito==ifrom.or.ito>nr) then
                        write (*, 5356) ifrom,ito
                        write (iofill, 5356) ifrom,ito
                        stop
                    end if
                    if (f(i)<0.0_eb.or.f(i)>1.0_eb) then
                        write (*, 5357) ifrom,ito,f(i)
                        write (iofill, 5357) ifrom,ito,f(i)
                        stop
                    end if
                    roomptr%heat_frac(ito) = f(i)
                end do

            else if (trim(type) == 'CEILING' .or. trim(type) == 'FLOOR') then
                counter1 = counter1 + 1

                compartment_id = ' '
                compartment_id = comp_id
                i1 = -101

                searching_3: do jj = 1, nr-1
                    roomptr => roominfo(jj)
                    if (trim(compartment_id) == trim(roomptr%name)) then
                        i1 = roomptr%compartment
                        exit searching_3
                    end if
                end do searching_3

                if (i1 == -101) then
                    write (*,'(a,a)') '***Error: COMP_ID not found. ', comp_id
                    write (iofill,'(a,a)') '***Error: COMP_ID not found. ', comp_id
                    stop
                end if

                compartment_id = ' '
                compartment_id = comp_ids(1)
                i2 = -101

                searching_4: do jj = 1, nr-1
                    roomptr => roominfo(jj)
                    if (trim(compartment_id) == trim(roomptr%name)) then
                        i2 = roomptr%compartment
                        exit searching_4
                    end if
                end do searching_4

                if (i2 == -101) then
                    write (*,'(a,a)') '***Error: COMP_ID not found. ', comp_ids(1)
                    write (iofill,'(a,a)') '***Error: COMP_ID not found. ', comp_ids(1)
                    stop
                end if

                if (i1<1.or.i2<1.or.i1>nr.or.i2>nr) then
                    write (*,5345) i1, i2
                    write (iofill,5345) i1, i2
                    stop
                end if

                i_vconnections(counter1,w_from_room) = i1
                i_vconnections(counter1,w_from_wall) = 2
                i_vconnections(counter1,w_to_room) = i2
                i_vconnections(counter1,w_to_wall) = 1
            end if

        end do countloop

    end if conn_flag

5356 format ('***Error: Bad CONN input. CONN specification error in compartment pairs: ',2i3)
5357 format ('***Error: Bad CONN input. Error in fraction for CONN:',2i3,f6.3)
5345 format ('***Error: Bad VHEAT input. A referenced compartment does not exist')



    contains

    subroutine set_conn_defaults

    comp_id           = 'NULL'
    comp_ids(:)       = 'NULL'
    f(:)              = -101._eb
    type              = 'NULL'

    end subroutine set_conn_defaults

    end subroutine read_conn


    ! --------------------------- ISOF --------------------------------------------
    subroutine read_isof(lu)

    integer, intent(in) :: lu

    integer :: ios, ii, icomp, jj, counter
    character(64) :: compartment_id

    type(visual_type), pointer :: sliceptr
    type(room_type), pointer :: roomptr

    real(eb) :: value
    character(64) :: comp_id
    namelist /ISOF/ comp_id, value

    ios = 1

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
            write(iofill, '(a,i3)') 'Error: Invalid specification in &ISOF inputs. Check &ISOF input, ' , counter
            stop
        end if
    end do isof_loop

    isof_flag: if (isofflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_isof_loop: do ii = 1, counter

            call checkread('ISOF',lu,ios)
            call set_isof_defaults
            read(lu,ISOF)

            compartment_id = ' '
            compartment_id = comp_id
            icomp = 0

            searching: do jj = 1, nr-1
                roomptr => roominfo(jj)
                if (trim(compartment_id) == trim(roomptr%name)) then
                    icomp = roomptr%compartment
                    exit searching
                end if
            end do searching

            nvisualinfo = nvisualinfo + 1
            sliceptr => visualinfo(nvisualinfo)
            sliceptr%vtype = 3
            sliceptr%value = value + 273.15
            sliceptr%roomnum = icomp

            if (sliceptr%roomnum<0.or.sliceptr%roomnum>nr-1) then
                write (*, 5404) counter
                write (iofill, 5404) counter
                stop
            end if

        end do read_isof_loop

    end if isof_flag

5404 format ('***Error: Invalid ISOF specification in visualization input ',i0)



    contains

    subroutine set_isof_defaults

    value                   = -101.0_eb
    comp_id                 = 'NULL'

    end subroutine set_isof_defaults

    end subroutine read_isof

    ! --------------------------- SLCF --------------------------------------------
    
    subroutine read_slcf(lu)

    integer, intent(in) :: lu
    
    integer :: ios, ii, jj, icomp, counter
    character(64) :: compartment_id

    type(room_type), pointer :: roomptr
    type(visual_type), pointer :: sliceptr

    real(eb) :: position
    character(64) :: domain,plane
    character(64) :: comp_id
    namelist /SLCF/ domain, plane, position, comp_id

    ios = 1

    rewind (unit=lu)
    input_file_line_number = 0

    ! Scan entire file to look for 'SLCF'
    slcf_loop: do
        call checkread ('SLCF',lu,ios)
        if (ios==0) slcfflag=.true.
        if (ios==1) then
            exit slcf_loop
        end if
        read(lu,SLCF,err=34,iostat=ios)
        counter=counter+1
34      if (ios>0) then
            write(iofill, '(a,i3)') 'Error: Invalid specification in &SLCF inputs. Check &SLCF input, ' , counter
            stop
        end if
    end do slcf_loop

    slcf_flag: if (slcfflag) then

        rewind (lu)
        input_file_line_number = 0

        ! Assign value to CFAST variables for further calculations
        read_slcf_loop: do ii = 1,counter

            call checkread('SLCF',lu,ios)
            call set_slcf_defaults
            read(lu,SLCF)

            nvisualinfo = nvisualinfo + 1
            sliceptr => visualinfo(nvisualinfo)
            if (trim(domain)=='2-D') then
                sliceptr%vtype = 1
            else if (trim(domain)=='3-D') then
                sliceptr%vtype = 2
            else
                write (*, 5403) counter
                write (iofill, 5403) counter
                stop
            end if

            compartment_id = ' '
            compartment_id = comp_id
            icomp = 0

            if (trim(compartment_id) /= 'NULL') then
                searching: do jj = 1, nr-1
                    roomptr => roominfo(jj)
                    if (trim(compartment_id) == trim(roomptr%name)) then
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
                if (sliceptr%roomnum<0.or.sliceptr%roomnum>nr-1) then
                    write (*, 5403) counter
                    write (iofill, 5403) counter
                    stop
                end if
                if (trim(plane) =='X') then
                    sliceptr%axis = 1
                    if (sliceptr%roomnum>0) then
                        roomptr => roominfo(sliceptr%roomnum)
                        if (sliceptr%position>roomptr%cwidth.or.sliceptr%position<0.0_eb) then
                            write (*, 5403) counter
                            write (iofill, 5403) counter
                            stop
                        end if
                    end if
                else if (trim(plane) =='Y') then
                    sliceptr%axis = 2
                    if (sliceptr%roomnum>0) then
                        roomptr => roominfo(sliceptr%roomnum)
                        if (sliceptr%position>roomptr%cdepth.or.sliceptr%position<0.0_eb) then
                            write (*, 5403) counter
                            write (iofill, 5403) counter
                            stop
                        end if
                    end if
                else if (trim(plane) =='Z') then
                    sliceptr%axis = 3
                    if (sliceptr%roomnum>0) then
                        roomptr => roominfo(sliceptr%roomnum)
                        if (sliceptr%position>roomptr%cheight.or.sliceptr%position<0.0_eb) then
                            write (*, 5403) counter
                            write (iofill, 5403) counter
                            stop
                        end if
                    end if
                else
                    write (*, 5403) counter
                    write (iofill, 5403) counter
                    stop
                end if

                ! 3-D slice
            else if (sliceptr%vtype==2) then
                sliceptr%roomnum = icomp
                if (sliceptr%roomnum<0.or.sliceptr%roomnum>nr-1) then
                    write (*, 5403) counter
                    write (iofill, 5403) counter
                    stop
                end if
            end if

        end do read_slcf_loop

    end if slcf_flag

5403 format ('***Error: Bad SLCF input. Invalid SLCF specification in visualization input ',i0)

    contains

    subroutine set_slcf_defaults

    domain                  = 'NULL'
    plane                   = 'NULL'
    position                = 0._eb
    comp_id                 = 'NULL'

    end subroutine set_slcf_defaults

    end subroutine read_slcf

    ! --------------------------- checkread ---------------------------------------
    subroutine checkread(name,lu,ios)

    ! look for the namelist variable name and then stop at that line.

    integer :: ii
    integer, intent(out) :: ios
    integer, intent(in) :: lu
    character(4), intent(in) :: name
    character(80) text
    ios = 1

    readloop: do
        read(lu,'(a)',end=10) text
        input_file_line_number = input_file_line_number + 1
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

    end module namelist_input_routines
